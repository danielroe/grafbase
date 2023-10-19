import { User } from '@grafbase/generated'

export default async function(user: User): Promise<string | null> {
    return `${user.firstName} ${user.lastName}`
}
