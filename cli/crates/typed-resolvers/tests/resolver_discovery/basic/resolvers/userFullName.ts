import { User } from '@grafbase/generated'

export default async function fullName(user: User): Promise<string | null> {
    return `${user.firstName} ${user.lastName}`
}
