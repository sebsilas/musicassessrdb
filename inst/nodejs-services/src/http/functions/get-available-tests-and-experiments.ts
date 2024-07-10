import { db } from '../../core/rds'

export const handler = async (event) => {
    let result = {}
    let hasTests = false
    let hasExperiments = false
    let message = ""
    console.log(event)
    try {
        const userId = JSON.parse(event.body).user_id
        const getUser = await db` select * from users where user_id = ${userId}`
        const appName = JSON.parse(event.body).app_name ?? ""

        if (!getUser.length) {
            throw new Error("User does not exist!")
        }

        if (appName == "singpause") {

            result["tests"] = await db` select * from tests where active = true and  test_name  = 'SAA' order by test_id`
            result["experiments"] = []
            result["message"] = `You have successfully collected tests and experiments for user_id ${userId}!`
            return {
                statusCode: 200,
                body: JSON.stringify(result
                ),
            };

        }
        const getDemographics = await db` select * from demographics where user_id = ${userId}`
        const getExperiments = await db` select * from experiments_user_permissions where user_id = ${userId} and action = 'Allow'`
        hasTests = getDemographics.count > 0 ? true : false
        hasExperiments = getExperiments.count > 0 ? true : false
        const tests = hasTests ? await db` select * from tests where active = true order by test_id` : []
        const experiments = hasTests && hasExperiments ? await db` select * from experiments where active = true order by experiment_id` : []
        result["tests"] = tests ?? []
        result["experiments"] = experiments ?? []
        result["message"] = hasTests ? `You have successfully collected tests and experiments for user_id ${userId}!` : `The user ${userId} has not filled out the demographic format yet, therefore there are no tests or experiements available.`


    } catch (error) {
        console.error('Database connection error', error);
    }

    return {
        statusCode: 200,
        body: JSON.stringify(result
        ),
    };
};