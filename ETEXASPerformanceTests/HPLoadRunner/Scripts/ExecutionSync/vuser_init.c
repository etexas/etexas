vuser_init()
{
	//Generate unique username
	lr_save_string(lr_eval_string("{groupName}{vUserId}{timestamp}"), "username");
	lr_log_message("Username is :%s\n", lr_eval_string("{username}"));
	
	//********** LOAD LOGIN PAGE *****************
	lr_start_transaction("Load Login Page");

	web_add_auto_header("Accept-Language", 
		"en-US,en;q=0.5");

	web_url("etexas-test.internal.harmonia.com", 
		"URL=http://etexas-test.internal.harmonia.com/", 
		"Resource=0", 
		"RecContentType=text/html", 
		"Referer=", 
		"Snapshot=t32.inf", 
		"Mode=HTML", 
		LAST);

	web_url("index.jsp", 
		"URL=http://etexas-test.internal.harmonia.com/ui/index.jsp", 
		"Resource=0", 
		"RecContentType=text/html", 
		"Referer=http://etexas-test.internal.harmonia.com/", 
		"Snapshot=t33.inf", 
		"Mode=HTML", 
		EXTRARES, 
		"Url=../webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_01.css", "Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all.css", ENDITEM, 
		"Url=../webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_02.css", "Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all.css", ENDITEM, 
		"Url=../app/Globals.js?_dc=1480711358709", ENDITEM, 
		LAST);

	web_url("login.html", 
		"URL=http://etexas-test.internal.harmonia.com/login.html", 
		"Resource=0", 
		"RecContentType=text/html", 
		"Referer=http://etexas-test.internal.harmonia.com/ui/index.jsp", 
		"Snapshot=t34.inf", 
		"Mode=HTML", 
		LAST);

	web_set_sockets_option("SSL_VERSION", "2&3");

	web_add_auto_header("Accept-Language", 
		"en-US,en;q=0.5");

	lr_think_time(40);

	web_url("exclamation.png", 
		"URL=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/images/form/exclamation.png", 
		"Resource=1", 
		"RecContentType=image/png", 
		"Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_01.css", 
		"Snapshot=t42.inf", 
		LAST);

	web_url("tool-sprites.png", 
		"URL=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/images/tools/tool-sprites.png", 
		"Resource=1", 
		"RecContentType=image/png", 
		"Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_01.css", 
		"Snapshot=t43.inf", 
		LAST);
	
	lr_end_transaction("Load Login Page", LR_AUTO);
	//********** END LOAD LOGIN PAGE *****************
	
	lr_think_time(23);
	
	//********** REGISTRATION *****************
	lr_start_transaction("Registration");

		
	web_submit_data("register", 
		"Action=http://etexas-test.internal.harmonia.com/rest/user/register", 
		"Method=POST", 
		"RecContentType=application/json", 
		"Referer=http://etexas-test.internal.harmonia.com/login.html", 
		"Snapshot=t48.inf", 
		"Mode=HTML", 
		"EncodeAtSign=YES", 
		ITEMDATA, 
		"Name=username", "Value={username}", ENDITEM, 
		"Name=password", "Value=password", ENDITEM, 
		"Name=confirmPassword", "Value=password", ENDITEM, 
		"Name=email", "Value={username}@harmonia.com", ENDITEM, 
		"Name=firstName", "Value=new", ENDITEM, 
		"Name=lastName", "Value=user", ENDITEM, 
		"Name=organization", "Value=harmonia", ENDITEM, 
		LAST);


	lr_end_transaction("Registration", LR_AUTO);
	//********** END REGISTRATION *****************

	web_add_auto_header("Accept-Language", 
		"en-US,en;q=0.5");

	lr_think_time(143);
	
	//********** LOGIN *****************
	lr_start_transaction("Login");

	//Save the authentication token returned in the response
	web_reg_save_param_ex("ParamName=Token", SEARCH_FILTERS, "Scope=Body", LAST);
	
	web_submit_data("login", 
		"Action=http://etexas-test.internal.harmonia.com/rest/user/login", 
		"Method=POST", 
		"RecContentType=text/html", 
		"Referer=http://etexas-test.internal.harmonia.com/login.html", 
		"Snapshot=t49.inf", 
		"Mode=HTML", 
		ITEMDATA, 
		"Name=j_username", "Value={username}", ENDITEM, 
		"Name=j_password", "Value=password", ENDITEM, 
		LAST);
	
	//Save the portion of the URL that will contain the credentials for subsequent calls
	lr_save_string(lr_eval_string("http://{username}:{Token}@{hostName}.internal.harmonia.com"), "authURL");

	web_url("index.jsp_2", 
		"URL=http://etexas-test.internal.harmonia.com/ui/index.jsp", 
		"Resource=0", 
		"RecContentType=text/html", 
		"Referer=http://etexas-test.internal.harmonia.com/login.html", 
		"Snapshot=t50.inf", 
		"Mode=HTML", 
		EXTRARES, 
		"Url=../app/Globals.js?_dc=1480711593722", ENDITEM, 
		"Url=Overrides.js?_dc=1480711594785", ENDITEM, 
		"Url=../app/view/Viewport.js?_dc=1480711594785", ENDITEM, 
		"Url=../app/view/Header.js?_dc=1480711595875", ENDITEM, 
		"Url=../app/view/PerspectivesContainer.js?_dc=1480711595876", ENDITEM, 
		"Url=../app/view/Navigation.js?_dc=1480711595916", ENDITEM, 
		"Url=../app/view/perspectives/ExecutionsPerspective.js?_dc=1480711595924", ENDITEM, 
		"Url=../app/view/perspectives/DashboardPerspective.js?_dc=1480711595924", ENDITEM, 
		"Url=../app/view/perspectives/AppsPerspective.js?_dc=1480711595925", ENDITEM, 
		"Url=../app/view/perspectives/SimulationsPerspective.js?_dc=1480711595924", ENDITEM, 
		"Url=../app/GlobalFns.js?_dc=1480711597117", ENDITEM, 
		"Url=../app/view/components/TagGrid.js?_dc=1480711597213", ENDITEM, 
		"Url=../app/view/components/SimGrid.js?_dc=1480711597213", ENDITEM, 
		"Url=../app/view/components/ExecGrid.js?_dc=1480711597213", ENDITEM, 
		"Url=../app/view/components/HoverMenuButton.js?_dc=1480711598287", ENDITEM, 
		"Url=../app/controller/DashboardPerspectiveController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/ExecutionsPerspectiveController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/AppsPerspectiveController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/MainController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/components/lists/CommandsListController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/SimulationsPerspectiveController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/components/lists/DevicesListController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/components/lists/ExecutionsListController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/components/viewers/ExecutionViewerController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/components/viewers/MessagesViewerController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/controller/components/viewers/AppLogsViewerController.js?_dc=1480711598313", ENDITEM, 
		"Url=../app/view/components/tables/AppParamsTable.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/model/AppConfigParam.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/ItemSelector.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/model/ExecData.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/model/AppConfigData.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/dialogs/SimDeviceConfigDialog.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/AddRseForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/AddObuRuleForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/AddCellRuleForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/AddFixedCellDeviceForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/AddDetectorForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/dialogs/ConfigureSimDeviceDialog.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/CreateSimForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/UploadSimForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/CopySimForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/ExecSimForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/view/components/RenameSimForm.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/store/Apps.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/store/Devices.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/store/DeviceInstalledAppStore.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/store/SimStore.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/store/DetectorStore.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/store/ExecStore.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/store/ExecDataStore.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/store/CellTowerStore.js?_dc=1480711598504", ENDITEM, 
		"Url=../app/TemplateFns.js?_dc=1480711598509", ENDITEM, 
		"Url=../app/store/CommandStore.js?_dc=1480711598527", ENDITEM, 
		"Url=../app/view/components/lists/CommandsList.js?_dc=1480711598527", ENDITEM, 
		"Url=../app/view/components/dialogs/LaneChangeDialog.js?_dc=1480711598527", ENDITEM, 
		"Url=../app/view/components/dialogs/SpeedChangeDialog.js?_dc=1480711598527", ENDITEM, 
		"Url=../app/view/components/dialogs/SignalChangeDialog.js?_dc=1480711598527", ENDITEM, 
		"Url=../app/view/components/dialogs/InjectVehicleDialog.js?_dc=1480711598527", ENDITEM, 
		"Url=../app/store/VehicleTableStore.js?_dc=1480711598527", ENDITEM, 
		"Url=../app/view/components/viewers/DeviceViewer.js?_dc=1480711598544", ENDITEM, 
		"Url=../app/view/components/lists/DevicesList.js?_dc=1480711598544", ENDITEM, 
		"Url=../app/view/components/viewers/MessagesViewer.js?_dc=1480711598595", ENDITEM, 
		"Url=../app/store/ExecMessageStore.js?_dc=1480711598595", ENDITEM, 
		"Url=../app/view/components/tables/CommandHistoryTable.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/tables/DetectorsTable.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/tables/VehiclesTable.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/tables/LaneGeometryTable.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/tables/SignalsTable.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/viewers/ExecutionViewer.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/viewers/AppLogsViewer.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/tables/AppLogTable.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/dialogs/KeySelectionDialog.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/dialogs/AppSelectionDialog.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/view/components/dialogs/DeviceSelectionDialog.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/store/DetectorTableStore.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/store/SignalListStore.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/store/CommandHistoryTableStore.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/store/LaneGeometryListStore.js?_dc=1480711598618", ENDITEM, 
		"Url=../app/store/AppLogDevices.js?_dc=1480711598626", ENDITEM, 
		"Url=../app/store/AppLogApps.js?_dc=1480711598626", ENDITEM, 
		"Url=../app/store/AppLogKeys.js?_dc=1480711598626", ENDITEM, 
		"Url=../app/store/AppLog.js?_dc=1480711598626", ENDITEM, 
		"Url=../app/view/components/CellRuleBaseForm.js?_dc=1480711598873", ENDITEM, 
		"Url=../app/view/components/FixedCellDeviceBaseForm.js?_dc=1480711598877", ENDITEM, 
		"Url=../app/store/TemplateStore.js?_dc=1480711598981", ENDITEM, 
		"Url=../app/model/AppDefData.js?_dc=1480711599014", ENDITEM, 
		"Url=../app/model/DeviceData.js?_dc=1480711599163", ENDITEM, 
		"Url=../app/model/SimData.js?_dc=1480711599238", ENDITEM, 
		"Url=../app/model/DetectorData.js?_dc=1480711599250", ENDITEM, 
		"Url=../app/model/CellTowerData.js?_dc=1480711599266", ENDITEM, 
		"Url=../app/view/components/tables/ExecutionHistoryTable.js?_dc=1480711600330", ENDITEM, 
		"Url=../app/view/components/lists/ExecutionsList.js?_dc=1480711600330", ENDITEM, 
		"Url=../app/view/components/tables/ExecutionsTable.js?_dc=1480711600330", ENDITEM, 
		"Url=../app/view/components/dialogs/UploadAppForm.js?_dc=1480711600331", ENDITEM, 
		"Url=../app/view/components/tables/NativeAppGrid.js?_dc=1480711600331", ENDITEM, 
		"Url=../app/view/components/tables/BuiltInAppGrid.js?_dc=1480711600331", ENDITEM, 
		"Url=../app/view/components/dialogs/NativeAppForm.js?_dc=1480711600331", ENDITEM, 
		"Url=../app/view/components/tables/AppJarGrid.js?_dc=1480711600331", ENDITEM, 
		"Url=../app/view/components/dialogs/UploadAppJarForm.js?_dc=1480711600331", ENDITEM, 
		"Url=../app/view/components/tables/RemoteAppGrid.js?_dc=1480711600331", ENDITEM, 
		"Url=../app/view/components/ImageButton.js?_dc=1480711600331", ENDITEM, 
		"Url=../app/view/components/dialogs/FilterBySimDialog.js?_dc=1480711600332", ENDITEM, 
		"Url=../app/view/components/dialogs/FilterByTagDialog.js?_dc=1480711600332", ENDITEM, 
		"Url=../app/model/VehicleTableData.js?_dc=1480711601477", ENDITEM, 
		"Url=../app/model/MessageType.js?_dc=1480711601627", ENDITEM, 
		"Url=../app/view/RecordXTemplate.js?_dc=1480711601762", ENDITEM, 
		"Url=../app/view/components/CheckboxModel.js?_dc=1480711601781", ENDITEM, 
		"Url=../app/store/reader/DetectorJSONReader.js?_dc=1480711601888", ENDITEM, 
		"Url=../app/model/SignalListData.js?_dc=1480711601945", ENDITEM, 
		"Url=../app/model/CommandHistoryData.js?_dc=1480711602033", ENDITEM, 
		"Url=../app/model/LaneGeometryListData.js?_dc=1480711602038", ENDITEM, 
		"Url=../app/model/LogData.js?_dc=1480711602256", ENDITEM, 
		"Url=../app/model/SingleValue.js?_dc=1480711602264", ENDITEM, 
		"Url=../app/model/TemplateData.js?_dc=1480711604236", ENDITEM, 
		"Url=../app/model/DetectorTableData.js?_dc=1480711615322", ENDITEM, 
		"Url=../app/model/DetectorEvent.js?_dc=1480711622644", ENDITEM, 
		"Url=../app/model/DetectorArea.js?_dc=1480711622644", ENDITEM, 
		"Url=../app/model/LaneGeometryMovementsMap.js?_dc=1480711622644", ENDITEM, 
		"Url=../app/model/LaneGeometryNode.js?_dc=1480711622644", ENDITEM, 
		"Url=../app/model/LaneGeometryMovementEntry.js?_dc=1480711622752", ENDITEM, 
		"Url=../app/model/LaneGeometryMovement.js?_dc=1480711623753", ENDITEM, 
		"Url=../IMG/window_world_128.png", ENDITEM, 
		"Url=../IMG/window_up_128.png", ENDITEM, 
		"Url=../IMG/go_up_128.png", ENDITEM, 
		"Url=../IMG/go_copy_128.png", ENDITEM, 
		"Url={authURL}/rest/api/sims?projName=&_dc=1480711624556&page=1&start=0&limit=25", ENDITEM, 
		"Url={authURL}/rest/api/apps?_dc=1480711624562&page=1&start=0&limit=25", ENDITEM, 
		"Url={authURL}/rest/api/templateSims?_dc=1480711724683&page=1&start=0&limit=25", ENDITEM, 
		"Url={authURL}/rest/api/sims?_dc=1480711759921&page=1&start=0&limit=25", ENDITEM, 
		"Url={authURL}/rest/api/sims?_dc=1480711807465&page=1&start=0&limit=25", ENDITEM, 
		LAST);

	web_url("version_2", 
		"URL=http://etexas-test.internal.harmonia.com/rest/info/version?_dc=1480711594767", 
		"Resource=0", 
		"RecContentType=text/html", 
		"Referer=http://etexas-test.internal.harmonia.com/ui/index.jsp", 
		"Snapshot=t51.inf", 
		"Mode=HTML", 
		LAST);

	web_url("webstart", 
		"URL=http://etexas-test.internal.harmonia.com/rest/info/webstart?_dc=1480711594765", 
		"Resource=0", 
		"RecContentType=text/html", 
		"Referer=http://etexas-test.internal.harmonia.com/ui/index.jsp", 
		"Snapshot=t52.inf", 
		"Mode=HTML", 
		EXTRARES, 
		"Url=/resources/css/default/images/sim_config_16.png", "Referer=http://etexas-test.internal.harmonia.com/resources/css/default/app.css", ENDITEM, 
		"Url=/resources/css/default/images/button/default-toolbar-small-arrow.png", "Referer=http://etexas-test.internal.harmonia.com/resources/css/default/app.css", ENDITEM, 
		"Url=/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/images/button/default-toolbar-small-arrow.png", "Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_01.css", ENDITEM, 
		"Url=/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/images/toolbar/footer-more.png", "Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_01.css", ENDITEM, 
		"Url=/resources/css/default/images/list_clock_16.png", "Referer=http://etexas-test.internal.harmonia.com/resources/css/default/app.css", ENDITEM, 
		"Url=/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/images/form/checkbox.png", "Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_01.css", ENDITEM, 
		"Url=/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/images/loadmask/loading.gif", "Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_01.css", ENDITEM, 
		"Url=/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/images/grid/hd-pop.png", "Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_02.css", ENDITEM, 
		"Url=/resources/css/default/images/add_16.png", "Referer=http://etexas-test.internal.harmonia.com/resources/css/default/app.css", ENDITEM, 
		"Url=/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/images/form/trigger.png", "Referer=http://etexas-test.internal.harmonia.com/webjars/extjs/5.1.1/build/packages/ext-theme-neptune/build/resources/ext-theme-neptune-all_01.css", ENDITEM, 
		LAST);


	lr_end_transaction("Login", LR_AUTO);
	//********** END LOGIN *****************

	lr_think_time(27);
	
	return 0;
}
