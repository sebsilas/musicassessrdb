import { db } from '../../core/rds'

import { PutCommand, UpdateCommand } from "@aws-sdk/lib-dynamodb";
import { DynamoDBClient } from "@aws-sdk/client-dynamodb";



const dbClient = new DynamoDBClient({});


interface Job {
    jobId: string;
    name?: string;
    message?: string;
    status?: string;
}

const storeJob = async (job: Job) => {
    const createdAt = new Date().toISOString();
    const params = {
        TableName: 'jobs',
        Item: {
            jobId: job.jobId,
            name: job.name || "",
            message: job.message || "",
            status: job.status || "PENDING",
            createdAt: createdAt
        }
    };

    try {
        const command = new PutCommand(params);
        const response = await dbClient.send(command);
        console.log("Job stored successfully:", response);
    } catch (error) {
        console.error("Error storing job:", error);
    }
}

const updateJob = async (updateParams: Job) => {
    const params = {
        TableName: 'jobs',
        Key: {
            jobId: updateParams.jobId
        },
        UpdateExpression: 'SET message = :msg, #status = :sts',
        ExpressionAttributeValues: {
            ':msg': updateParams.message,
            ':sts': updateParams.status
        },
        ExpressionAttributeNames: {
            '#status': 'status'
        }
    };

    try {
        const command = new UpdateCommand(params);
        const response = await dbClient.send(command);
        console.log("Job updated successfully:", response);
    } catch (error) {
        console.error("Error updating job:", error);
    }
}

export const handler = async (event) => {
    let result = {}
    try {
        const userId = JSON.parse(event.body).user_id
        const item_id = JSON.parse(event.body).item_id.trim()
        const getUser = await db` select * from users where user_id = ${userId}`
        const item_bank_id = JSON.parse(event.body).item_bank_id
        const jobId = JSON.parse(event.body).job_id

        let item_bank_data
        console.log("item_bank_id", item_bank_id)
        console.log("item_id", item_id)

        if (!getUser.length) {
            result["message"] = "Something went wrong"
            throw new Error("User does not exist!")
        }


        storeJob({ jobId: jobId, name: "review items job" });


        const item_bank_name = await db`select item_bank_name  from item_banks where item_bank_id = ${item_bank_id} limit 1`

        if (!item_bank_name) {

            result["message"] = "Something went wrong"
            throw new Error("item_bank_name does not exist!")

        }

        const item_bank_name_table = `item_bank_${item_bank_name[0].item_bank_name}`
        console.log(item_bank_name_table)


        if (item_bank_name_table.includes('phrase')) {
            console.log(item_id)
            item_bank_data = await db`select distinct ib."item_id", "stimulus_abs_melody","stimulus_durations","onset","abs_melody", "durations", "melody" ,  ${item_bank_id} as item_bank_id  from ${db(item_bank_name_table)} ib
            left join  trials t on ib.item_id = t.item_id
            where ib.item_id = ${item_id}`

        } else {
            item_bank_data = await db`select ib."item_id", "stimulus_abs_melody","stimulus_durations","abs_melody", "durations", "melody" ,  ${item_bank_id} as item_bank_id  from ${db(item_bank_name_table)} ib
            left join  trials t on ib.item_id = t.item_id
            where ib.item_id = ${item_id}`
        }

        console.log(item_bank_data)
        const hasData = await db` select * from trials t 
        left join sessions s on t.session_id=s.session_id 
        where s.user_id = ${userId} and item_id = ${item_id} limit 1`


        if (!hasData.length) {
            console.log(JSON.stringify({ jobId: jobId, message: { new_items: item_bank_data } }))
            const new_items_id = await db`insert into new_items (user_id,item_id,item_bank_id,ranking,prediction_statistic,prediction_time,prediction_method) values (${userId},${item_id},${item_bank_id},1,null,now(),'user_selected') returning new_items_id`
            result["message"] = "You have successfully added new items"



            updateJob({
                jobId: jobId, message: JSON.stringify({
                    new_items: item_bank_data.map(item => ({
                        ...item,
                        new_items_id: new_items_id[0]?.new_items_id
                    }))
                }), status: "FINISHED"
            });
            return {
                statusCode: 200,
                body: JSON.stringify(result
                )
            }

        }

        const review_items_id = await db`insert into review_items (user_id,item_id,item_bank_id,ranking,prediction_statistic,prediction_time,prediction_method) values (${userId},${item_id},${item_bank_id},1,null,now(),'user_selected') returning review_items_id`

        updateJob({
            jobId: jobId, message: JSON.stringify({
                review_items: item_bank_data.map(item => ({
                    ...item,
                    review_items_id: review_items_id[0]?.review_items_id
                }))
            }), status: "FINISHED"
        });
        console.log(JSON.stringify({ jobId: jobId, message: { review_items: item_bank_data, review_items_id: review_items_id[0]?.review_items_id }, status: "FINISHED" }));
        result["message"] = "You have successfully added new review_items"


    } catch (error) {
        console.error('Database connection error', error);
    }

    return {
        statusCode: 200,
        body: JSON.stringify(result
        ),
    };
};