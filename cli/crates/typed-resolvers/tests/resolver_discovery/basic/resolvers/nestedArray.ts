import { User } from '@grafbase/generated'
import { ID } from '@grafbase/sdk'

export default async function comparisons(_: User, args: { otherUser: ID }): Promise<boolean[][]> {
    return [[]]
}


