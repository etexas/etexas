//   *****************************************************************************************************************************************
//   ****   PLEASE NOTE: This is a READ-ONLY representation of the actual script. For editing please press the "Develop Script" button.   ****
//   *****************************************************************************************************************************************

Library_eTexasTruClient()
{
	truclient_step("1", "Function Initial Page Load", "snapshot=eTexasTruClient_1.inf");
	{
		lr_start_transaction("Initial Page Load");
		truclient_step("1.1", "Navigate to 'etexas-test.internal.harmonia.com'", "snapshot=eTexasTruClient_1.1.inf");
		truclient_step("1.2", "Verify Login to eTEXAS 's 'Visible Text' Contain Login to eTEXAS", "snapshot=eTexasTruClient_1.2.inf");
		lr_end_transaction("Initial Page Load",0);
	}
	truclient_step("2", "Function Login", "snapshot=eTexasTruClient_2.inf");
	{
		lr_start_transaction("Login");
		truclient_step("2.1", "Click on Username textbox", "snapshot=eTexasTruClient_2.1.inf");
		truclient_step("2.2", "Type FuncArgs.username; in Username textbox", "snapshot=eTexasTruClient_2.2.inf");
		truclient_step("2.3", "Type ***************** in Password textbox", "snapshot=eTexasTruClient_2.3.inf");
		truclient_step("2.4", "Click on Login", "snapshot=eTexasTruClient_2.4.inf");
		truclient_step("2.5", "Verify User Dropdown 's 'Visible Text' Contain FuncArgs.username;", "snapshot=eTexasTruClient_2.5.inf");
		lr_end_transaction("Login",0);
	}
	truclient_step("3", "Function Logout", "snapshot=eTexasTruClient_3.inf");
	{
		lr_start_transaction("Logout");
		truclient_step("3.1", "Click on FuncArgs.username", "snapshot=eTexasTruClient_3.1.inf");
		truclient_step("3.2", "Click on Log Out menuitem", "snapshot=eTexasTruClient_3.2.inf");
		truclient_step("3.3", "Verify Login to eTEXAS 's 'Visible Text' Contain Login to eTEXAS", "snapshot=eTexasTruClient_3.3.inf");
		lr_end_transaction("Logout",0);
	}
	truclient_step("4", "Function Register", "snapshot=eTexasTruClient_4.inf");
	{
		truclient_step("4.1", "Click on Need an account? Sign up.", "snapshot=eTexasTruClient_4.1.inf");
		truclient_step("4.2", "Click on Username:* textbox", "snapshot=eTexasTruClient_4.2.inf");
		truclient_step("4.3", "Evaluate JavaScript code window.alert({user.username});", "snapshot=eTexasTruClient_4.3.inf");
		truclient_step("4.4", "Type user.username in Username:* textbox", "snapshot=eTexasTruClient_4.4.inf");
		truclient_step("4.5", "Type ************* in Password:* textbox", "snapshot=eTexasTruClient_4.5.inf");
		truclient_step("4.6", "Type ************* in Confirm Password:* textbox", "snapshot=eTexasTruClient_4.6.inf");
		truclient_step("4.7", "Type user.email in Email Address:* textbox", "snapshot=eTexasTruClient_4.7.inf");
		truclient_step("4.8", "Type user.firstName in First Name:* textbox", "snapshot=eTexasTruClient_4.8.inf");
		truclient_step("4.9", "Type user.lastName in Last Name:* textbox", "snapshot=eTexasTruClient_4.9.inf");
		truclient_step("4.10", "Type user.organization in Organization textbox", "snapshot=eTexasTruClient_4.10.inf");
		truclient_step("4.11", "Click on Register", "snapshot=eTexasTruClient_4.11.inf");
		truclient_step("4.12", "Click on OK", "snapshot=eTexasTruClient_4.12.inf");
	}
	truclient_step("5", "Function eTexasJS", "snapshot=eTexasTruClient_5.inf");
	{
		truclient_step("5.1", "Evaluate JavaScript code Utils.import('\\\\dev\\D...\\eTexasJS.js');", "snapshot=eTexasTruClient_5.1.inf");
	}
	truclient_step("6", "Function createUserJS", "snapshot=eTexasTruClient_6.inf");
	{
		truclient_step("6.1", "Evaluate JavaScript code Global.user = new Object...etterString(10);", "snapshot=eTexasTruClient_6.1.inf");
	}
	truclient_step("7", "Function InitialJS", "snapshot=eTexasTruClient_7.inf");
	{
		truclient_step("7.1", "Call Function TruClientGlobal.GlobalJS", "snapshot=eTexasTruClient_7.1.inf");
		truclient_step("7.2", "Call Function eTexasTruClient.eTexasJS", "snapshot=eTexasTruClient_7.2.inf");
	}

	return 0;
}

Library_TruClientGlobal()
{
	truclient_step("1", "Function GlobalJS", "snapshot=TruClientGlobal_1.inf");
	{
		truclient_step("1.1", "Evaluate JavaScript code Utils.import('\\\\dev\\D...\\globalJS.js');", "snapshot=TruClientGlobal_1.1.inf");
	}

	return 0;
}
