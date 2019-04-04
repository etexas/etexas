package com.harmonia.qa.ETEXASWebQATests.suites;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.harmonia.qa.ETEXASWebQATests.ETexasBaseData;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.AddCellularDeviceProfileTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.AddOBUDeviceProfileTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.CompositeCellularOptionsTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.CompositeConfigurationOptionsTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.ConfigureACompositeTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.ConfigureCellDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.ConfigureOBUDeviceProfileTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.DeleteCellularDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.DeleteOBUDeviceProfileTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.EditCellularDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests.EditOBUDeviceProfileTest;
import com.harmonia.qa.ETEXASWebQATests.ReportManagement.ConfigureReportingTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.AddCellTowerTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.AddDetectorTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.AddFixedCellularDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.AddRSEDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.ConfigureASimulationTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.ConfigureFixedCellDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.ConfigureRSEDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.DeleteCellTowerTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.DeleteDetectorTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.DeleteFixedCellularDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.DeleteRSEDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.EditCellTowerTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.EditDetectorTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.EditFixedCellularDeviceTest;
import com.harmonia.qa.ETEXASWebQATests.SimulationSettingsTests.EditRSEDeviceTest;
import com.harmonia.qa.Entities.Utilities.CoolDown;

/**
 * Test suite listing the eTexas automated tests related to devices (addition,
 * deletion, configuration)
 *
 * @author llaroussini
 */
/* @formatter:off */
@RunWith(Suite.class)
@SuiteClasses(
        {
            ETexasBaseData.class,
            //Configure Simulation Tests
            ConfigureASimulationTest.class, //TC-095
            //Cell Tower Tests
            AddCellTowerTest.class, //TC-075, ITC-056 - TODO: ITC not yet re-enabled
            EditCellTowerTest.class, //TC-076, ITC-057 - TODO: ITC not yet re-enabled
            DeleteCellTowerTest.class, //TC-077, ITC-058 - TODO: ITC not yet re-enabled
            //Detector Tests
            AddDetectorTest.class, //TC-054, ITC-035
            EditDetectorTest.class, //TC-097, ITC-074
            DeleteDetectorTest.class, //TC-055, ITC-036 TODO: ITC not yet re-enabled
            //Fixed Cellular Device Tests
            AddFixedCellularDeviceTest.class, //TC-071, ITC-052
            EditFixedCellularDeviceTest.class, //TC-079, ITC-060
            ConfigureFixedCellDeviceTest.class, //TC-074 ITC-055
            DeleteFixedCellularDeviceTest.class, //TC-080 TODO: ITC-061 not yet re-enabled
            //RSE Device Tests
            AddRSEDeviceTest.class, //TC-039, ITC-006
            EditRSEDeviceTest.class, //TC-096, ITC-075
            ConfigureRSEDeviceTest.class, //TC-041, ITC-008
            DeleteRSEDeviceTest.class, //TC-066, ITC-047 - TODO: ITC not yet re-enabled
            //Configure Composite Tests
            ConfigureACompositeTest.class, //TC-098
            CompositeConfigurationOptionsTest.class, //TC-011, ITC-012
            CompositeCellularOptionsTest.class, //TC-078, ITC-059
            //Cellular Device Profile Tests
            AddCellularDeviceProfileTest.class, //TC-068, ITC-049
            EditCellularDeviceTest.class, //TC-069, ITC-050
            ConfigureCellDeviceTest.class, //TC-073, ITC-054
            DeleteCellularDeviceTest.class, //TC-070
            //OBU Device Tests
            AddOBUDeviceProfileTest.class, //TC-040, ITC-007
            EditOBUDeviceProfileTest.class, //TC-099, ITC-081
            ConfigureOBUDeviceProfileTest.class, //TC-072, ITC-053
            DeleteOBUDeviceProfileTest.class, //TC-067, ITC-048 - TODO: ITC not yet re-enabled
            //Reporting Tests
            ConfigureReportingTest.class, //TC-065, ITC-046 - TODO: ITC not yet re-enabled
            CoolDown.class,
        }
        )
public class SimulationAndCompositeSettingsTests {}
/* @formatter:on*/
