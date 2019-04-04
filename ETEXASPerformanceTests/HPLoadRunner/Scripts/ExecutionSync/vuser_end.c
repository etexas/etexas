vuser_end()
{
	//********** LOGOUT *****************
	lr_start_transaction("Logout");

		web_url("login.html_2", 
		"URL=http://etexas-test.internal.harmonia.com/login.html", 
		"Resource=0", 
		"Referer=http://etexas-test.internal.harmonia.com/ui/index.jsp", 
		"Snapshot=t56.inf", 
		"Mode=HTML", 
		EXTRARES, 
		"Url=/webjars/extjs/5.1.1/build/ext-all-debug.js", ENDITEM, 
		LAST);

	web_url("version_3", 
		"URL=http://etexas-test.internal.harmonia.com/rest/info/version?_dc=1480711886051", 
		"Resource=0", 
		"RecContentType=text/html", 
		"Referer=http://etexas-test.internal.harmonia.com/login.html", 
		"Snapshot=t57.inf", 
		"Mode=HTML", 
		LAST);

	web_url("blogrss_2", 
		"URL=http://etexas-test.internal.harmonia.com/rest/info/blogrss?_dc=1480711886055&numEntries=2", 
		"Resource=0", 
		"RecContentType=text/html", 
		"Referer=http://etexas-test.internal.harmonia.com/login.html", 
		"Snapshot=t58.inf", 
		"Mode=HTML", 
		LAST);

	lr_end_transaction("Logout", LR_AUTO);
	//********** END LOGOUT *****************
	
	return 0;
}
