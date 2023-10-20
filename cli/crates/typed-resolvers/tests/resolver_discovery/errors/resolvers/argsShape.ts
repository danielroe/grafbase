import { User } from '@grafbase/generated'
import { Context } from '@grafbase/sdk'

export default async function friends(_user: User, _args: FriendsArgs, _context: Context): Promise<User[]> {
    return []
}

