//   *****************************************************************************************************************************************
//   ****   PLEASE NOTE: This is a READ-ONLY representation of the actual script. For editing please press the "Develop Script" button.   ****
//   *****************************************************************************************************************************************

vuser_init()
{
	truclient_step("1", "Call Function eTexasTruClient.InitialJS", "snapshot=Init_1.inf");
	truclient_step("2", "Call Function eTexasTruClient.createUserJS", "snapshot=Init_2.inf");
	truclient_step("3", "Evaluate JavaScript code TC.setParam('username', ... user.password);", "snapshot=Init_3.inf");
	truclient_step("4", "Call Function eTexasTruClient.Initial Page Load", "snapshot=Init_4.inf");
	truclient_step("5", "Call Function eTexasTruClient.Register", "snapshot=Init_5.inf");

	return 0;
}
