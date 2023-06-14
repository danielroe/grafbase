use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Write},
    ops::Deref,
};

use dynaql_parser::{
    types::{
        Directive, Field, FragmentDefinition, FragmentSpread, InlineFragment, Selection,
        TypeCondition,
    },
    Positioned,
};
use dynaql_value::{Name, Value};

/// Serialize a list of [`Selection`]s into a GraphQL query string.
///
/// The serializer is specifically tailored for the [`graphql::Resolver`](super::Resolver), as it
/// has logic to prepend/remove namespaced prefixes to global types, and injects `__typename`
/// fields into queries that need it for the resolver to properly parse the returned data.
pub struct Serializer<'a, 'b, W> {
    /// The prefix string to strip from any global type, before serializing the query.
    prefix: Option<&'a str>,

    /// Buffer used to write operation string to.
    buf: &'a mut W,

    /// Global list of fragment definitions, to allow the serializer to embed the definitions of
    /// any fragments used within the query.
    fragment_definitions: HashMap<&'b Name, &'b FragmentDefinition>,

    /// Internal tracking of all fragment spreads used within the execution document.
    /// These are linked to the known `fragment_definitions` to embed the required fragment
    /// definitions in the document.
    fragment_spreads: HashSet<&'b Name>,

    /// Internal tracking of indentation to pretty-print query.
    indent: usize,

    /// A list of serialized variable references.
    ///
    /// This allows the caller to pass along the relevant variable values to the upsteam server.
    variable_references: HashSet<&'a Name>,
}

impl<'a, 'b, W> Serializer<'a, 'b, W> {
    pub fn new(
        prefix: Option<&'a str>,
        fragment_definitions: HashMap<&'b Name, &'b FragmentDefinition>,
        buf: &'a mut W,
    ) -> Self {
        Serializer {
            prefix,
            buf,
            fragment_definitions,
            fragment_spreads: HashSet::new(),
            indent: 0,
            variable_references: HashSet::new(),
        }
    }

    /// Get an iterator over variable references the serializer has serialized.
    ///
    /// This list will be empty, until [`Serializer::query()`] or [`Serializer::mutation()`] is
    /// called.
    pub fn variable_references(&self) -> impl Iterator<Item = &Name> {
        self.variable_references.iter().copied()
    }
}

impl<'a: 'b, 'b: 'a, 'c: 'a, W: Write> Serializer<'a, 'b, W> {
    /// Serialize query.
    ///
    /// # Errors
    ///
    /// Returns an error if writing to the buffer fails.
    pub fn query(&mut self, selections: impl Iterator<Item = &'c Selection>) -> Result<(), Error> {
        self.write_str("query")?;
        self.serialize_selections(selections)?;
        self.serialize_fragment_definitions()
    }

    /// Serialize mutation.
    ///
    /// # Errors
    ///
    /// Returns an error if writing to the buffer fails.
    pub fn mutation(
        &mut self,
        selections: impl Iterator<Item = &'c Selection>,
    ) -> Result<(), Error> {
        self.write_str("mutation")?;
        self.serialize_selections(selections)?;
        self.serialize_fragment_definitions()
    }

    fn serialize_selection(&mut self, selection: &'c Selection) -> Result<(), Error> {
        use Selection::{Field, FragmentSpread, InlineFragment};

        match selection {
            Field(Positioned { node, .. }) => self.serialize_field(node),
            FragmentSpread(Positioned { node, .. }) => self.serialize_fragment_spread(node),
            InlineFragment(Positioned { node, .. }) => self.serialize_inline_fragment(node),
        }
    }

    fn serialize_field(&mut self, field: &'a Field) -> Result<(), Error> {
        self.indent()?;

        // Alias
        //
        // <https://graphql.org/learn/queries/#aliases>
        if let Some(Positioned { node, .. }) = &field.alias {
            self.write_str(node)?;
            self.write_str(": ")?;
        }

        // Field name
        self.write_str(field.name.as_str())?;

        // Arguments
        self.serialize_arguments(&field.arguments)?;

        // Directives
        {
            let directives = field.directives.iter().map(|v| &v.node);
            self.serialize_directives(directives)?;
        }

        // Selection Sets
        {
            let selections = field.selection_set.deref().items.iter().map(|v| &v.node);
            self.serialize_selections(selections)?;
        }

        self.write_str("\n")
    }

    /// Arguments
    ///
    /// <https://graphql.org/learn/queries/#arguments>
    fn serialize_arguments(
        &mut self,
        arguments: &'a [(Positioned<Name>, Positioned<Value>)],
    ) -> Result<(), Error> {
        if arguments.is_empty() {
            return Ok(());
        }

        self.write_str("(")?;

        let mut arguments = arguments.iter().map(|(k, v)| (&k.node, &v.node)).peekable();

        while let Some((name, value)) = arguments.next() {
            // If the argument references a variable, we track it so that the caller knows which
            // variable values are needed to execute the document.
            if let Value::Variable(name) = value {
                self.variable_references.insert(name);
            }

            self.write_str(name)?;
            self.write_str(": ")?;
            self.write_str(value.to_string())?;

            if arguments.peek().is_some() {
                self.write_str(", ")?;
            }
        }

        self.write_str(")")
    }

    /// Selection Sets
    ///
    /// <https://spec.graphql.org/June2018/#sec-Selection-Sets>
    fn serialize_selections(
        &mut self,
        selections: impl Iterator<Item = &'c Selection>,
    ) -> Result<(), Error> {
        let mut selections = selections.peekable();

        if selections.peek().is_none() {
            return Ok(());
        }

        self.write_str(" {\n")?;
        self.indent += 1;

        // We always inject `__typename` into every selection set (except for the root). This is
        // needed in specific cases for Grafbase to correctly link responses back to known types.
        //
        // While we technically don't need to embed the field in _every_ selection set for Grafbase
        // to function properly, it's simpler to do so, and follows precedence set by clients such
        // as Apollo[1].
        //
        // [1]: https://www.apollographql.com/docs/ios/fetching/type-conditions/#type-conversion
        if self.indent > 1 {
            self.indent()?;
            self.write_str("__typename\n")?;
        }

        for selection in selections {
            self.serialize_selection(selection)?;
        }

        // Clean-up before closing the set.
        self.indent = self.indent.saturating_sub(1);

        self.writeln_str("}\n")
    }

    fn serialize_directives(
        &mut self,
        directives: impl Iterator<Item = &'c Directive>,
    ) -> Result<(), Error> {
        for directive in directives {
            self.write_str(" @")?;
            self.write_str(directive.name.as_str())?;
            self.serialize_arguments(&directive.arguments)?;
        }

        Ok(())
    }

    /// Fragment Spread
    ///
    /// <https://spec.graphql.org/June2018/#FragmentSpread>
    fn serialize_fragment_spread(&mut self, fragment: &'c FragmentSpread) -> Result<(), Error> {
        let fragment_name = &fragment.fragment_name;

        self.indent()?;
        self.write_str("... ")?;
        self.write_str(fragment_name.as_str())?;

        self.fragment_spreads
            .insert(fragment_name.as_ref().into_inner());

        let directives = fragment.directives.iter().map(|v| &v.node);
        self.serialize_directives(directives)?;
        self.write_str("\n")
    }

    /// Inline Fragment
    ///
    /// <https://spec.graphql.org/June2018/#sec-Inline-Fragments>
    fn serialize_inline_fragment(&mut self, fragment: &'c InlineFragment) -> Result<(), Error> {
        let type_condition = fragment.type_condition.as_ref().map(|v| &v.node);
        let directives = fragment.directives.iter().map(|v| &v.node);
        let selections = fragment.selection_set.deref().items.iter().map(|v| &v.node);

        self.indent()?;
        self.write_str("...")?;

        self.serialize_fragment_inner(type_condition, directives, selections)
    }

    fn serialize_fragment_definitions(&mut self) -> Result<(), Error> {
        if self.fragment_spreads.is_empty() {
            return Ok(());
        }

        for name in self.fragment_spreads.clone() {
            // If a spread references an unknown definition, the query will fail, but the failure
            // will be reported by the GraphQL resolver, not this serializer.
            if let Some(definition) = self.fragment_definitions.get(&name) {
                self.serialize_fragment_definition(name, definition)?;
            }
        }

        Ok(())
    }

    fn serialize_fragment_definition(
        &mut self,
        name: &Name,
        definition: &'c FragmentDefinition,
    ) -> Result<(), Error> {
        self.write_str("fragment ")?;
        self.write_str(name)?;

        let type_condition = &definition.type_condition.node;
        let directives = definition.directives.iter().map(|v| &v.node);
        let selections = definition
            .selection_set
            .deref()
            .items
            .iter()
            .map(|v| &v.node);

        self.serialize_fragment_inner(Some(type_condition), directives, selections)
    }

    fn serialize_fragment_inner(
        &mut self,
        type_condition: Option<&'c TypeCondition>,
        directives: impl Iterator<Item = &'c Directive>,
        selections: impl Iterator<Item = &'c Selection>,
    ) -> Result<(), Error> {
        if let Some(condition) = type_condition {
            self.write_str(" on ")?;

            // We remove the `prefix` from condition types, as these are local to Grafbase, and
            // should not be sent to the upstream server.
            if let Some(prefix) = self.prefix {
                self.write_str(condition.on.as_str().replace(prefix, ""))?;
            }
        }

        self.serialize_directives(directives)?;
        self.serialize_selections(selections)
    }

    fn indent(&mut self) -> Result<(), Error> {
        self.buf.write_str(&"\t".repeat(self.indent))?;
        Ok(())
    }

    fn writeln_str(&mut self, s: impl AsRef<str>) -> Result<(), Error> {
        self.indent()?;
        self.write_str(s)
    }

    fn write_str(&mut self, s: impl AsRef<str>) -> Result<(), Error> {
        self.buf.write_str(s.as_ref())?;
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Fmt(#[from] fmt::Error),
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::*;

    macro_rules! set_snapshot_suffix {
        ($($expr:expr),*) => {
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_suffix(format!($($expr,)*));
            let _guard = settings.bind_to_scope();
        }
    }

    #[rstest]
    #[case::one("query { foo }")]
    #[case::many("query { foo\nbar }")]
    fn base_fields(#[case] input: &str) {
        set_snapshot_suffix!("{}", input);
        insta::assert_snapshot!(serialize(input));
    }

    #[rstest]
    #[case::one("query { foo(a: \"\") }")]
    #[case::many("query { foo(a: \"bar\", baz: true) }")]
    fn field_arguments(#[case] input: &str) {
        set_snapshot_suffix!("{}", input);
        insta::assert_snapshot!(serialize(input));
    }

    #[rstest]
    #[case::one_bare("query { foo @include }")]
    #[case::one_arguments("query { foo @include(if: true) }")]
    #[case::many_bare("query { foo @include @deprecated }")]
    #[case::many_arguments("query { foo @include(if: true) @exclude(if: 42) }")]
    #[case::many_mixed("query { foo @include(if: true) @deprecated @exclude(if: 42) }")]
    fn field_directives(#[case] input: &str) {
        set_snapshot_suffix!("{}", input);
        insta::assert_snapshot!(serialize(input));
    }

    #[rstest]
    #[case::one("query { foo { bar } }")]
    #[case::many("query { foo { bar baz } qux { quux } }")]
    fn field_selections(#[case] input: &str) {
        set_snapshot_suffix!("{}", input);
        insta::assert_snapshot!(serialize(input));
    }

    #[rstest]
    #[case::one("query { ... foo }")]
    #[case::many("query { ... fooBar @deprecated }")]
    fn fragment_spread(#[case] input: &str) {
        set_snapshot_suffix!("{}", input);
        insta::assert_snapshot!(serialize(input));
    }

    #[rstest]
    #[case::cond("query { ... on Foo { bar baz } }")]
    #[case::directive("query { ... @include(if: $foo) { bar } }")]
    #[case::cond_and_directive("query { ... on Foo @deprecated { baz } }")]
    fn inline_fragment(#[case] input: &str) {
        set_snapshot_suffix!("{}", input);
        insta::assert_snapshot!(serialize(input));
    }

    #[test]
    fn complex() {
        let input = r#"
        query {
          repository(name: "api", owner: "grafbase") {
            issueOrPullRequest(number: 2129) {
              ... on GithubIssue {
                id
              }

              ... on GithubPullRequest {
                id
                changedFiles
              }
            }
          }
        }"#;

        insta::assert_snapshot!(serialize(input));
    }

    #[test]
    fn fragment_definitions() {
        let input = r#"
        query {
          repository(name: "api", owner: "grafbase") {
            pullRequest(number: 2129) {
              ...fields
            }
          }
        }

        fragment fields on GithubPullRequest {
          id
          changedFiles
        }"#;

        insta::assert_snapshot!(serialize(input));
    }

    fn serialize(input: &str) -> String {
        let mut buf = String::new();
        let (selections, fragment_definitions) = input_to_selections(input);
        let fragments = fragment_definitions.iter().map(|(k, v)| (k, v)).collect();

        let mut serializer = Serializer::new(Some("Github"), fragments, &mut buf);

        if input.trim_start().starts_with("query") {
            serializer.query(selections.iter()).unwrap();
        } else if input.trim_start().starts_with("mutation") {
            serializer.mutation(selections.iter()).unwrap();
        } else {
            panic!("invalid input data");
        }

        buf
    }

    fn input_to_selections(input: &str) -> (Vec<Selection>, HashMap<Name, FragmentDefinition>) {
        let document = dynaql_parser::parse_query(input).unwrap();
        let operation = document
            .operations
            .iter()
            .next()
            .unwrap()
            .1
            .clone()
            .into_inner();

        let selections = operation
            .selection_set
            .into_inner()
            .items
            .into_iter()
            .map(Positioned::into_inner)
            .collect();

        let fragments = document
            .fragments
            .into_iter()
            .map(|(k, v)| (k, v.into_inner()))
            .collect();

        (selections, fragments)
    }
}
