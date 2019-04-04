package com.harmonia.qa.ETEXASWebQATests.AppManagementTests;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.UserJarApp;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.AppFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.apps.ETexasAppUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.ApplicationParametersModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.JARApplicationsPartialPage;

/**
 * Executes steps of View JAR of Applications Parameters test, TC-093
 *
 * @author llaroussini
 */
public class ViewJARofApplicationsParametersTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Jar app used in test case
     */
    private UserJarApp jarApp;

    /**
     * The App file which will be uploaded
     */
    private File uploadedAppFile;

    /**
     * The name of the file which will be uploaded
     */
    private String testFileName = "msg-rx-app-2.0-SNAPSHOT.jar";

    /**
     * The file display name in UI
     */
    private String fileDisplayName = "MsgRxApp";

    /**
     * File path to access JAR file
     */
    private String applicationFilePath = "src/test/resources/";

    /**
     * Test setup.
     *
     * @throws IOException if errors are encountered reading the file name/path
     *         - likely an issue with file permissions
     */
    @Before
    public void warmUp() throws IOException {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get user and JAR App
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        uploadedAppFile = FileUtils.getFile(applicationFilePath, testFileName);
        jarApp = AppFactory.getUserJarApp(fileDisplayName, DeviceType.OBU, uploadedAppFile);
        jarApp.setUser(testUser);
        ETexasEntityManager.addEntities(testUser, jarApp);

        //Register user and create JAR App
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasAppUtils.createJARApp(jarApp, uploadedAppFile);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-093
     */
    @Test
    public void viewJarAppParamsExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Click the Applications button in the top menu bar.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        AppsPage appsPage = simPage.clickApps();

        //Verify the Connected Vehicle Applications page is displayed.
        Assert.assertTrue("Connected Vehicle Applications header is not displayed as expected after clicking Applications.", appsPage.isAppsHeaderDisplayed());

        //Verify the following tabs are displayed along the top of the page: Embedded Applications, JAR Applications, Native Applications, and Remote Applications.
        appsPage.checkAllTabs();

        //Click the JAR Applications tab.
        JARApplicationsPartialPage jarTab = appsPage.clickJARAppsTab();

        //Select any listed JAR application.
        jarTab.selectRow(fileDisplayName, true);

        //Verify the Parameters button is enabled.
        Assert.assertFalse("The Parameters button is not enabled as expected when an application is selected.", jarTab.isParametersBtnDisabled());

        //Click the Parameters button.
        ApplicationParametersModal paramModal = jarTab.clickParameters();

        //Verify an Application Parameters modal is displayed.
        Assert.assertTrue("Application Parameters modal is not displayed as expected after clicking Parameters.", paramModal.isAppParametersHeaderDisplayed());

        //Verify a ‘?’ icon and an ‘x’ icon are displayed in the top right corner of the modal.
        paramModal.checkAppParametersHeaderIcons();

        //Click the ‘?’ icon.
        paramModal.clickAppParametersHelpIcon();

        //Verify an Application Parameters Help modal is displayed with details associated to application parameters.
        paramModal.checkHelpModal();

        //Verify an OK button is displayed.
        Assert.assertTrue("OK button is not displayed as expected in Application Parameters help modal.", paramModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        paramModal.clickHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("Application Parameters help modal is still displayed after clicking OK.", paramModal.isAppParametersHelpHeaderDisplayed());

        //Verify a table of app parameters, if any exist, is displayed in the Application Parameters modal; If no parameters exist, verify text is displayed indicating no parameters are defined for the application.
        if (jarApp.getParameters().size() == 0) {
            Assert.assertFalse(
                    "The Application Parameters table displayed in the Application Parameters modal for application in JAR named: " + fileDisplayName + ", which is NOT expected to have parameters.",
                    paramModal.isNoParametersAreaDisplayed());
        }
        else {
            Assert.assertTrue("No parameters are displayed in the Application Parameters modal for applicaiton in JAR named: " + fileDisplayName + ", which is expected to have parameters",
                    paramModal.isAppParameterTableDisplayed());
        }

        //Verify the following columns are displayed in the Application Parameters modal: Name and Default Value.
        paramModal.checkColumnHeaderCells();

        //Verify a Close button is displayed at the bottom of the modal
        Assert.assertTrue("Close button is not displayed as expected in Application Parameters modal.", paramModal.isCloseBtnDisplayed());

        //If any parameters exist for the selected app, verify they are listed with the correct parameter name and default value in the table.
        paramModal.checkAppParameters(jarApp);

        //Click the Close button.
        paramModal.clickCloseButton();

        //Verify the Application Parameters modal is no longer displayed.
        Assert.assertFalse("The Application Parameters modal is still displayed ater clicking the Close button.", paramModal.isAppParametersHeaderDisplayed());

        //Verify the application is still selected, if not, select the application.
        if (jarTab.isAppInJARRowSelected(fileDisplayName) == false) {
            jarTab.selectAppInJAR(fileDisplayName, true);
        }

        //Click the Parameters button.
        jarTab.clickParameters();

        //Click the ‘x’ icon.
        paramModal.clickCloseIcon();

        //Verify the Application Parameters modal is no longer displayed.
        Assert.assertFalse("The Application Parameters modal is still displayed ater clicking the 'x' icon.", paramModal.isAppParametersHeaderDisplayed());

        //Log Out
        jarTab.logout(testUser);
    }
}
