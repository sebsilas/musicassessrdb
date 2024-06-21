# Boilerplate for deploying R lambda functions and http api using serverless framework


## Prerequisites

- [Nodejs v16.x+](https://nodejs.org/)
- [Serverless Framework v3.0+](https://serverless.com/)
``` npm install -g serverless ```


### How to Deploy a New Lambda Function?

  - Add your Lambda function code to /R/mylambdafunction.R
  - Duplicate content in the same file inst/serverless/functions/api.yaml from line 1 to 11.
  - Adjust values based on the commented hints (function name, path in the API, etc.).
  - Make sure you have pushed changes to Github
  - Make sure to force the Dockerfile to reinstall the *musicassessrdb* package on rebuild

## Deploy your functions and endpoints:
```
serverless deploy --stage dev --aws-profile musicassessr --region eu-central-1 --verbose
```
