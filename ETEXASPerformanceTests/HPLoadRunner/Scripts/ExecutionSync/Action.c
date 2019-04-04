Action()
{
	
	
	
	//********** CREATE SIMULATION FROM TEMPLATE *****************
	lr_start_transaction("Create Simulation from Template");

	//Save the simulation id returned in the response
	web_reg_save_param_ex("ParamName=simId", "LB=\"simId\":", "RB=}", SEARCH_FILTERS, "Scope=Body", LAST);
	
	//Generate a simulation name incorporating the iteration number to prevent duplicate names on subsequent iterations
	lr_save_string(lr_eval_string("test_sim_{simNameSuffix}"), "simName");
	
	/*
	TODO - investigate further following analysis and discussion with developers
	Template is hardcoded as exam_01 to see how the system handles concurrent runs of identical executions.
	This value could be randomly selected from the list of available templates, or we could iterate over all 20 in 20 different runs of this vUser.
	*/
	lr_save_string(lr_eval_string("exam_01"), "templateName");
	
	web_submit_data("sims",
		"Action={authURL}/rest/api/sims", 
		"Method=POST", 
		"RecContentType=application/json", 
		"Referer=http://etexas-test.internal.harmonia.com/ui/index.jsp", 
		"Snapshot=t53.inf", 
		"Mode=HTML", 
		ITEMDATA, 
		"Name=name", "Value={simName}", ENDITEM, 
		"Name=template", "Value={templateName}", ENDITEM, 
		EXTRARES, 
		"Url={authURL}/resources/css/default/images/edit_16.png", "Referer=http://etexas-test.internal.harmonia.com/resources/css/default/app.css", ENDITEM, 
		"Url={authURL}/resources/css/default/images/detector_16.png", "Referer=http://etexas-test.internal.harmonia.com/resources/css/default/app.css", ENDITEM, 
		"Url={authURL}/resources/css/default/images/config_16.png", "Referer=http://etexas-test.internal.harmonia.com/resources/css/default/app.css", ENDITEM, 
		"Url={authURL}/resources/css/default/images/device_16.png", "Referer=http://etexas-test.internal.harmonia.com/resources/css/default/app.css", ENDITEM, 
		LAST);


	lr_end_transaction("Create Simulation from Template", LR_AUTO);
	//********** END CREATE SIMULATION FROM TEMPLATE *****************
	
	lr_think_time(4);
	
	//********** CREATE EXECUTION WITHOUT SEED *****************
	lr_start_transaction("Create Execution Without Seed");

	//Save the execution id returned in the response
	web_reg_save_param_ex("ParamName=execId", "LB=\"id\":", "RB=,", SEARCH_FILTERS, "Scope=Body", LAST);
	
	web_custom_request("execs", 
		"URL={authURL}/rest/api/sims/{simId}/execs/", 
		"Method=POST", 
		"Resource=0", 
		"RecContentType=application/json", 
		"Referer=http://etexas-test.internal.harmonia.com/ui/index.jsp", 
		"Snapshot=t54.inf", 
		"Mode=HTML", 
		"EncType=", 
		LAST);

	lr_end_transaction("Create Execution Without Seed", LR_AUTO);
	//********** END CREATE EXECUTION WITHOUT SEED *****************
	
	lr_think_time(45);
	
	//********** FINISH EXECUTION *****************
	lr_rendezvous("Finish_Execution_Sync"); //Synchronize all vUsers here
	lr_start_transaction("Finish Execution");

		web_custom_request("finish", 
		"URL={authURL}/rest/api/sims/{simId}/execs/{execId}/finish", 
		"Method=PUT", 
		"Resource=0", 
		"RecContentType=text/plain", 
		"Referer=http://etexas-test.internal.harmonia.com/ui/index.jsp", 
		"Snapshot=t55.inf", 
		"Mode=HTML", 
		LAST);

	lr_end_transaction("Finish Execution", LR_AUTO);
	//********** END FINISH EXECUTION *****************

	


	return 0;
}