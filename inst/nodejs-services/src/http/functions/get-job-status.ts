import { APIGatewayProxyEvent, APIGatewayProxyResult } from 'aws-lambda';
import { DynamoDBClient, GetItemCommand } from '@aws-sdk/client-dynamodb';
import * as crypto from 'crypto';
import { unmarshall } from '@aws-sdk/util-dynamodb';

const client = new DynamoDBClient({});

export const handler = async (event: APIGatewayProxyEvent): Promise<APIGatewayProxyResult> => {
    const filename = JSON.parse(event.body).filename
    let jobId = JSON.parse(event.body).job_id

                
    if (!filename && !jobId) {

        return {
            statusCode: 400,
            body: JSON.stringify({ message: 'Filename or Jobid is required' }),
        };


    }
    
    if(!jobId){
        jobId = crypto.createHash('md5').update(filename).digest('hex');
    }

    const params = {
        TableName: 'jobs',
        Key: {
            jobId: { S: jobId }
        },
    };

    console.log('DynamoDB query parameters:', params);

    try {
        const data = await client.send(new GetItemCommand(params));

        console.log('DynamoDB response:', data);

        if (!data.Item) {
            return {
                statusCode: 404,
                body: JSON.stringify({ message: 'Item not found' }),
            };
        }

        const item = unmarshall(data.Item);
        return {
            statusCode: 200,
            body: JSON.stringify(item),
        };
    } catch (error) {
        console.error('DynamoDB error:', error);
        return {
            statusCode: 500,
            body: JSON.stringify({ message: 'Internal Server Error', error: error.message }),
        };
    }

};
