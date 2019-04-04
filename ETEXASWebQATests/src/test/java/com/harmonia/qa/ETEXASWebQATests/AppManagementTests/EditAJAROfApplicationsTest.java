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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.EditJARApplicationModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.JARApplicationsPartialPage;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Executes steps of Edit a JAR Of Applications test, TC-092
 *
 * @author llaroussini
 */
public class EditAJAROfApplicationsTest extends ETexasAfterTestResetTestBase {

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

        //Get test user, jar file, jar app
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        uploadedAppFile = FileUtils.getFile(applicationFilePath, testFileName);
        jarApp = AppFactory.getUserJarApp(RandomStringGenerator.nextLetterString(10), DeviceType.OBU, uploadedAppFile);
        jarApp.setUser(testUser);
        jarApp.setName(fileDisplayName);
        ETexasEntityManager.addEntities(testUser, jarApp);

        //Register user
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);

        //Create JAR app in UI
        ETexasAppUtils.createJARApp(jarApp, uploadedAppFile);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-092
     *
     * @throws IOException if errors are encountered reading the file name/path
     *         - likely an issue with file permissions
     */
    @Test
    public void editJarAppExternalTest() throws IOException {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Click the Applications button in the top menu bar.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        AppsPage appsPage = simPage.clickApps();

        //Verify the Connected Vehicle Applications page is displayed.
        Assert.assertTrue("The Connected Vehicle Application page is not displayed as expected after clicking 'Applications'.", appsPage.isAppsHeaderDisplayed());

        //Verify the following tabs are displayed along the top of the page: Embedded Applications, JAR Applications, Native Applications, and Remote Applications.
        appsPage.checkAllTabs();

        //Click the JAR Applications tab.
        JARApplicationsPartialPage jarAppsPage = appsPage.clickJARAppsTab();

        //Select any listed JAR of applications.
        jarAppsPage.selectRow(fileDisplayName, true);

        //Verify the Edit button is enabled.
        Assert.assertFalse("Edit button is not enabled as expected when JAR application is selected.", jarAppsPage.isEditBtnDisabled());

        //Click the Edit button.
        EditJARApplicationModal editModal = jarAppsPage.clickEdit();

        //Verify an Edit JAR Application modal is displayed.
        Assert.assertTrue("Edit JAR Application modal is not displayed as expected when Edit is clicked.", editModal.isEditJARAppHeaderDisplayed());

        //Verify a ‘?’ icon and an ‘x’ icon are displayed in the top right corner of the modal.
        editModal.checkEditJARAppHeaderIcons();

        //Click the ‘?’ icon.
        editModal.clickEditJARAppHelpIcon();

        //Verify an Edit JAR Application Help modal is displayed with details associated to editing a JAR application.
        editModal.checkHelpModal();

        //Verify an OK button is displayed.
        Assert.assertTrue("OK button is not displayed as expected in the Edit JAR Application Help modal.", editModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        editModal.clickHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("Edit Native Application Help modal is still displayed after clicking OK.", editModal.isEditJARAppHelpHeaderDisplayed());

        //Verify the following text boxes are displayed in the Edit JAR Application modal: JAR Name and Name.
        editModal.checkFieldsDisplayed();

        //Verify the JAR Name text box is populated with the name of the selected JAR and the Name text box is populated with the name of selected application within the selected JAR.
        editModal.checkFieldValues(jarApp, fileDisplayName);

        //Verify the following buttons are displayed at the bottom of the Edit JAR Application modal: Update, Reset, and Cancel.
        editModal.checkBtns();

        //Automation only: Get new name values
        String newJarName = RandomStringGenerator.nextLetterString(5);
        String newAppName = RandomStringGenerator.nextLetterString(5);

        //Make a change to the JAR name value.
        editModal.setJARName(newJarName);

        //Make a change to the application name value.
        editModal.setAppName(newAppName);

        //Click the Reset button.
        editModal.clickResetBtn();

        //Verify both name values are reset to their original values.
        editModal.checkFieldValues(jarApp, fileDisplayName);

        //Make a change to the JAR name value and the application name value.
        editModal.setJARName(newJarName);
        editModal.setAppName(newAppName);

        //Click the Cancel button.
        editModal.clickCancelBtn();

        //Verify the Edit JAR Application modal is no longer displayed.
        Assert.assertFalse("Edit JAR Application modal is still displayed after clicking Cancel.", editModal.isEditJARAppHeaderDisplayed());

        //Verify the JAR of applications is unchanged in the table of application JARs.
        Assert.assertTrue("Original JAR Application no longer displayed despite cancelling update.", jarAppsPage.isAppDisplayed(fileDisplayName));

        //Verify the application is still selected, if not, select the application.
        if (jarAppsPage.isAppInJARRowSelected(fileDisplayName) == false) {
            jarAppsPage.selectAppInJAR(fileDisplayName, true);
        }

        //Click the Edit button.
        jarAppsPage.clickEdit();

        //Verify the Edit JAR Application modal is displayed.
        Assert.assertTrue("Edit JAR Application modal is not displayed as expected when Edit is clicked.", editModal.isEditJARAppHeaderDisplayed());

        //Make a change to the JAR name value and the application name value.
        editModal.setJARName(newJarName);
        editModal.setAppName(newAppName);

        //Click the ‘x’ icon.
        editModal.clickCloseIcon();

        //Verify the Edit JAR Application modal is no longer displayed.
        Assert.assertFalse("Edit JAR Application modal is still displayed after clicking the Close icon.", editModal.isEditJARAppHeaderDisplayed());

        //Verify the JAR of applications is unchanged in the table of application JARs.
        Assert.assertTrue("Original JAR Application no longer displayed despite closing prior to updating.", jarAppsPage.isAppDisplayed(fileDisplayName));

        //Verify the application is still selected, if not, select the application.
        if (jarAppsPage.isAppInJARRowSelected(fileDisplayName) == false) {
            jarAppsPage.selectAppInJAR(fileDisplayName, true);
        }

        //Click the Edit button.
        jarAppsPage.clickEdit();

        //Verify the Edit JAR Application modal is displayed.
        Assert.assertTrue("Edit JAR Application modal is not displayed as expected when Edit is clicked.", editModal.isEditJARAppHeaderDisplayed());

        //Make a change to the JAR name value and the application name value.
        editModal.setJARName(newJarName);
        editModal.setAppName(newAppName);

        //Click the Update button.
        editModal.clickUpdate();

        //Verify the Edit JAR Application modal is no longer displayed.
        Assert.assertFalse("Edit JAR Application modal is still displayed after clicking Update.", editModal.isEditJARAppHeaderDisplayed());

        //Verify the JAR name is updated in the table of application JARs.
        Assert.assertFalse("Original JAR Application still displayed despite succesfully updating the app.", jarAppsPage.isAppDisplayed(fileDisplayName));
        Assert.assertTrue("Updated JAR name not displayed despite succesfully updating the app.", jarAppsPage.isJARDisplayed(newJarName));

        //Verify the application name within the JAR is updated in the table of application JARs.
        Assert.assertTrue("Updated Application name not displayed despite succesfully updating the app.", jarAppsPage.isAppDisplayed(newAppName));

        //Verify the ID of the JAR remains unchanged.
        Assert.assertEquals("Application ID value was changed after the JAR application was updated.", jarApp.getID(), jarAppsPage.getJARAppID(newAppName));

        //Automation only: update entity manager
        UserJarApp origApp = ETexasEntityManager.getJARApp(jarApp.getName());
        origApp.setName(newJarName);

        //Log out
        jarAppsPage.logout(testUser);
    }
}