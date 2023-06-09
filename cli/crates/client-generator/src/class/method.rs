use std::{borrow::Cow, fmt};

use crate::{
    r#type::{Property, PropertyValue, TypeKind},
    Block, FunctionBody,
};

use super::Privacy;

#[derive(Debug)]
pub struct Method {
    inner: FunctionBody,
    privacy: Option<Privacy>,
}

impl Method {
    pub fn new(name: impl Into<Cow<'static, str>>, body: Block) -> Self {
        let inner = FunctionBody {
            name: name.into(),
            params: Vec::new(),
            returns: None,
            body,
        };

        Self { inner, privacy: None }
    }

    pub fn returns(mut self, r#type: impl Into<TypeKind>) -> Self {
        self.inner.returns = Some(r#type.into());
        self
    }

    pub fn push_param(mut self, key: impl Into<Cow<'static, str>>, value: impl Into<PropertyValue>) -> Self {
        self.inner.params.push(Property::new(key, value));
        self
    }

    #[must_use]
    pub fn public(mut self) -> Self {
        self.privacy = Some(Privacy::Public);
        self
    }

    #[must_use]
    pub fn protected(mut self) -> Self {
        self.privacy = Some(Privacy::Protected);
        self
    }

    #[must_use]
    pub fn private(mut self) -> Self {
        self.privacy = Some(Privacy::Private);
        self
    }
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(privacy) = self.privacy {
            write!(f, "{privacy} ")?;
        }

        self.inner.fmt(f)
    }
}
