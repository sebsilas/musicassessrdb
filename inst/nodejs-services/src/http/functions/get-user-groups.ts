import { db } from '../../core/rds'

export const handler = async (event) => {
    let result = {}
    try {
        const userId = JSON.parse(event.body).user_id
        const getUser = await db` select * from users where user_id = ${userId}`

        if (!getUser.length) {
            result["message"] = "Something went wrong"
            throw new Error("User does not exist!")
        }
        result["groups"] = await db` select * from groups  where group_id in (select group_id from users_groups  where user_id  = ${userId})`

    } catch (error) {
        result["message"] = "Something went wrong"
        console.error('Database connection error', error);
    }

    return {
        statusCode: 200,
        body: JSON.stringify(result
        ),
    };
};