package com.harmonia.qa.ETEXASWebQATests.suites;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.harmonia.qa.ETEXASWebQATests.ETexasBaseData;
import com.harmonia.qa.Entities.Utilities.CoolDown;

/**
 * Test suite listing the eTexas automated tests related to basic management of
 * executions
 *
 * @author llaroussini
 */
/* @formatter:off */
@RunWith(Suite.class)
@SuiteClasses(
		{
			ETexasBaseData.class,
			//TODO - update once executions are fully implemented in version 3.0
			//			StartNewExecutionTest.class, //TC-015, ITC-013
			//			RepeatAnExecutionTest.class, //TC-057, ITC-038
			//			DeleteAnExistingExecutionTest.class, //TC-016, ITC-014 (No applicable negative tests)
			//			ResumeAnExecutionTest.class, //TC-017, ITC-015 (No applicable negative tests)
			//			FilterExecutionsByStatusTest.class, //TC-018, ITC-016 (No applicable negative tests)
			//			FilterExecutionsByNameTest.class, //TC-038, ITC-017 (No applicable negative tests)
			//			FilterExecutionsByIDTest.class, //TC-062
			//			FilterExecutionsByDateTest.class, //TC-063
			//			RunExecutionToCompleteTest.class, //TC-021
			//			ViewCompletedExecutionDetailsTest.class, //TC-019
			CoolDown.class,
		}
		)
public class ETexasBasicExecutionTests {}
/* @formatter:on*/
