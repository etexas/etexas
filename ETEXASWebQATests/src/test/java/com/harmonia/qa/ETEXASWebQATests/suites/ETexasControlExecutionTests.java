package com.harmonia.qa.ETEXASWebQATests.suites;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.harmonia.qa.ETEXASWebQATests.ETexasBaseData;
import com.harmonia.qa.Entities.Utilities.CoolDown;

/**
 * Test suite listing the eTexas automated tests related to controlling
 * (stepping-through) executions
 *
 * @author llaroussini
 */
/* @formatter:off */
@RunWith(Suite.class)
@SuiteClasses(
		{
			ETexasBaseData.class,
			//TODO - update once executions are fully implemented in version 3.0
			//			StepThroughInProgressExecutionTest.class, //TC-022, ITC-018
			//			CommandQueueOptionsTest.class, //TC-048
			//			SpeedChangeTest.class, //TC-049
			//			LaneChangeTest.class, //TC-050
			//			SignalChangeTest.class, //TC-051
			//			InjectVehicleTest.class, //TC-052
			//			ViewMessagesInExecutionTest.class, //TC-025
			//			FilterMessagesInInProgressExecutionTest.class, //TC-056, ITC-037
			//			ViewDevicesInExecutionTest.class, //TC-053
			CoolDown.class,
		}
		)
public class ETexasControlExecutionTests {}
/* @formatter:on*/
