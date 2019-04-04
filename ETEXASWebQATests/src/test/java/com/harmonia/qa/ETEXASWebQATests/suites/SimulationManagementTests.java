package com.harmonia.qa.ETEXASWebQATests.suites;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.harmonia.qa.ETEXASWebQATests.ETexasBaseData;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.CopyACompositeTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.CopyASimulationTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.CreateANewCompositeWithTemplateSimulationTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.CreateANewCompositeWithUploadedSimulation;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.CreateANewTemplateSimulationInACompositeTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.CreateANewUploadedSimulationInACompositeTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.DeleteCompositeSimulationTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.DeleteSimulationInACompositeTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.RenameCompositeTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationManagementTests.RenameSimulationInACompositeTest;
import com.harmonia.qa.Entities.Utilities.CoolDown;

/**
 * Test suite listing the eTexas automated tests related to simulations
 *
 * @author llaroussini
 */
/* @formatter:off */
@RunWith(Suite.class)
@SuiteClasses(
        {
            ETexasBaseData.class,
            CreateANewCompositeWithTemplateSimulationTest.class, //TC-082, ITC-072
            CreateANewCompositeWithUploadedSimulation.class, //TC-100, ITC-076
            CreateANewTemplateSimulationInACompositeTest.class, //TC-004, ITC-003
            CreateANewUploadedSimulationInACompositeTest.class, //TC-006, ITC-005 - TODO: ITC not yet re-enabled
            CopyACompositeTest.class, //TC-101, ITC-082
            CopyASimulationTest.class, //TC-005, ITC-004
            RenameCompositeTest.class, //TC-083, ITC-077
            RenameSimulationInACompositeTest.class, //TC-061, ITC-042 - TODO: ITC not yet re-enabled
            DeleteCompositeSimulationTest.class, //TC-085
            DeleteSimulationInACompositeTest.class, //TC-010
            CoolDown.class,
        }
        )
public class SimulationManagementTests {}
/* @formatter:on*/
