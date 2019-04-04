//   *****************************************************************************************************************************************
//   ****   PLEASE NOTE: This is a READ-ONLY representation of the actual script. For editing please press the "Develop Script" button.   ****
//   *****************************************************************************************************************************************

Action()
{
	truclient_step("1", "Call Function eTexasTruClient.Initial Page Load", "snapshot=Action_1.inf");
	truclient_step("2", "Call Function eTexasTruClient.Login", "snapshot=Action_2.inf");
	truclient_step("3", "Call Function eTexasTruClient.Logout", "snapshot=Action_3.inf");

	return 0;
}
