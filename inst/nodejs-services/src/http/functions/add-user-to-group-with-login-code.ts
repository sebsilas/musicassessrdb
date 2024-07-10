import { db } from '../../core/rds'

export const handler = async (event) => {
    let result = {}
    try {
        const userId = JSON.parse(event.body).user_id
        const login_code = JSON.parse(event.body).login_code
        const getUser = await db` select * from users where user_id = ${userId}`

        if (!getUser.length) {
            result["message"] = "Something went wrong"
            throw new Error("User does not exist!")
        }

        const getGroupId = await db` select * from groups where sign_up_code = ${login_code} limit 1`

        if (!getGroupId.length) {
            result["message"] = "Something went wrong"
            throw new Error("Login code is incorrect")
        }
        const getUserGroup = await db` select * from users_groups where user_id = ${userId} and group_id= ${getGroupId[0].group_id}  limit 1`

        if (getUserGroup.length) {
            result["message"] = "The user has already joined this group"
            throw new Error("The user has already joined this group")
        }
        await db`insert into users_groups
        (group_id, user_id)
      values
        (${getGroupId[0].group_id}, ${userId}) returning user_id`;

        result["message"] = `You have successfully joined the group ${getGroupId[0].group_name}`
    } catch (error) {
        console.error('Database connection error', error);
    }

    return {
        statusCode: 200,
        body: JSON.stringify(result
        ),
    };
};