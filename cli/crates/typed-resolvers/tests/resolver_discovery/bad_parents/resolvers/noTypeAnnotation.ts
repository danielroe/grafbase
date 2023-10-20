import { User } from '@grafbase/generated'
import { Context } from '@grafbase/sdk'

export default async function noTypeAnnotation(parent): Promise<string> {
    return "???"
}
