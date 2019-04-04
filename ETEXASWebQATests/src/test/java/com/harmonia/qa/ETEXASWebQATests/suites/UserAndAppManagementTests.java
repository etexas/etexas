package com.harmonia.qa.ETEXASWebQATests.suites;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import com.harmonia.qa.ETEXASWebQATests.ETexasBaseData;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.CreateAJAROfApplicationsTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.CreateANativeApplicationTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.CreateARemoteApplicationTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.DeleteAJARofApplicationsTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.DeleteANativeApplicationTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.DeleteARemoteApplicationTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.EditAJAROfApplicationsTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.EditANativeApplicationTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.EditARemoteApplicationTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.ViewEmbeddedApplicationsTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.ViewJARofApplicationsParametersTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.ViewNativeApplicationParametersTest;
import com.harmonia.qa.ETEXASWebQATests.AppManagementTests.ViewRemoteApplicationParametersTest;
import com.harmonia.qa.ETEXASWebQATests.PageTests.LandingPageTests;
import com.harmonia.qa.ETEXASWebQATests.UserManagementTests.LoginTest;
import com.harmonia.qa.ETEXASWebQATests.UserManagementTests.UserRegistrationTest;
import com.harmonia.qa.Entities.Utilities.CoolDown;

/**
 * Test suite listing the basic eTexas automated tests - registration/login/app
 * management/report viewing
 *
 * @author llaroussini
 * @author rsmith
 */
/* @formatter:off */
@RunWith(Suite.class)
@SuiteClasses(
        {
            ETexasBaseData.class,
            LandingPageTests.class,
            UserRegistrationTest.class, //TC-001, ITC-001 TODO Re-enable ITC-001 after BUG 13728 fixed
            LoginTest.class, //TC-002, ITC-002 TODO Re-enable ITC-002 after Simulations and Composites modals/methods are updated
            //Native Application Tests
            CreateANativeApplicationTest.class, //TC-042, ITC-029
            EditANativeApplicationTest.class, //TC-086
            ViewNativeApplicationParametersTest.class, //TC-087
            DeleteANativeApplicationTest.class, //TC-088
            //Remote Application Tests
            CreateARemoteApplicationTest.class, //TC-043, ITC-030
            EditARemoteApplicationTest.class, //TC-089, ITC-066
            ViewRemoteApplicationParametersTest.class, //TC-090
            DeleteARemoteApplicationTest.class,//TC-091
            //JAR of Applications Tests
            CreateAJAROfApplicationsTest.class, //TC-044, ITC-031
            EditAJAROfApplicationsTest.class, //TC-092
            ViewJARofApplicationsParametersTest.class, //TC-093
            DeleteAJARofApplicationsTest.class, //TC-094
            //Embedded Application Tests
            ViewEmbeddedApplicationsTest.class, //TC-045
            //TODO re-enable tests after updating based off new eTEXAS 3.0
            //ViewingLogDetailsTest.class, //TC-046
            //ViewingVehicleAppData.class, //TC-058
            //ViewingMessageAppData.class, //TC-059
            //ViewingReportMOEAppDataTest.class, //TC-060
            CoolDown.class,
        }
        )
public class UserAndAppManagementTests {}
/* @formatter:on*/
