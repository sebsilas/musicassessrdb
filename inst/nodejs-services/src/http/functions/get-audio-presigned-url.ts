import { PutObjectCommand, S3Client } from "@aws-sdk/client-s3";
import { getSignedUrl } from "@aws-sdk/s3-request-presigner";

const s3Client = new S3Client();

export const handler = async (event) => {
    const bucketName = process.env.SOURCE_BUCKET
    const { filename, metadata } = JSON.parse(event.body);

    if (filename === undefined || filename.trim() === "") {
        return {
            statusCode: 400,
            body: JSON.stringify({
                message: "filename is required and cannot be an empty string",
            }),
        };
    }

    try {
        const command = new PutObjectCommand({
            Bucket: bucketName,
            Key: `${filename}.wav`,
            ContentType: "audio/wav",
            ACL: "public-read",
            Metadata: metadata // Include metadata here
        });

        const signedUrl = await getSignedUrl(s3Client, command, { expiresIn: 3600 });

        return {
            statusCode: 200,
            body: JSON.stringify({
                message: "Generated pre-signed URL",
                url: signedUrl,
            }),
        };
    } catch (error) {
        return {
            statusCode: 500,
            body: JSON.stringify({
                message: "Error generating pre-signed URL",
                error: error.message,
            }),
        };
    }
};
