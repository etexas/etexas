package com.harmonia.qa.ETEXASWebQATests.suites;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.harmonia.qa.ETEXASWebQATests.ETexasBaseData;
import com.harmonia.qa.ETEXASWebQATests.IntegrationTests.AppManagementIT;
import com.harmonia.qa.ETEXASWebQATests.IntegrationTests.UserServicesIT;
import com.harmonia.qa.Entities.Utilities.CoolDown;

/**
 * Test suite listing the eTexas integrations tests
 *
 * @author abishop
 */
/* @formatter:off */
@RunWith(Suite.class)
@SuiteClasses(
		{
			ETexasBaseData.class,
			UserServicesIT.class, //WS-TC042, WS-TC043
			AppManagementIT.class, //WS-TC050, WS-TC-044, WS-TC011, WS-TC072, WS-TC015, WS-TC017
			//TODO - update for version 3.0
			//SimulationIT.class, //WS-TC001, WS-TC044
			CoolDown.class,
		}
		)
public class ETexasIntegrationTests {}
/* @formatter:on*/
