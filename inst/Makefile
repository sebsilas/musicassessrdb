PROFILE=seb
STAGE=dev

nodejs: install_and_build_nodejs deploy_nodejs

install_and_build_nodejs:
	@cd nodejs-services/src && yarn install && yarn build

deploy_nodejs:
	@sls deploy  --config serverless-nodejs.yaml --stage $(STAGE) --aws-profile $(PROFILE)

deploy:
	@sls deploy  --stage $(STAGE) --aws-profile $(PROFILE)	

.PHONY: nodejs install_and_build_nodejs deploy deploy_nodejs