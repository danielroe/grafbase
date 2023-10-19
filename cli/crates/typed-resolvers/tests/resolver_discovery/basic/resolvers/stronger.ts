import { User } from '@grafbase/generated'
import { ID } from '@grafbase/sdk'

export default async function stronger(_: User, args: { otherUser: ID }): Promise<boolean> {
    return true // you're the strongest
}

