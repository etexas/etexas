package com.harmonia.qa.ETEXASWebQATests.AppManagementTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.AppFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.apps.ETexasAppUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.EditNativeApplicationModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.NativeAppsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.NativeAppsPartialPage.NativeAppColumn;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Executes steps of Edit a Native Application test, TC-086
 *
 * @author llaroussini
 */
public class EditANativeApplicationTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * User Native App used throughout the test case
     */
    private UserNativeApp app;

    /**
     * Native App String name used throughout the test case
     */
    private String appName;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user and app entity
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        app = AppFactory.getUserNativeApp(true); //Get a random native app
        appName = app.getName();
        app.setUser(testUser);
        ETexasEntityManager.addEntities(testUser, app);

        //Register user and create native app
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasAppUtils.createNativeApp(app);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-086: Edit a Native Application
     */
    @Test
    public void editNativeApplicationExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Click the Applications button in the top menu bar.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        AppsPage appsPage = simPage.clickApps();

        //Verify the Connected Vehicle Applications page is displayed.
        Assert.assertTrue("Connected Vehicle Applications header is not displayed as expected after clicking Applications.", appsPage.isAppsHeaderDisplayed());

        //Verify the following tabs are displayed along the top of the page: Embedded Applications, JAR Applications, Native Applications, and Remote Applications.
        appsPage.checkAllTabs();

        //Click the Native Applications tab.
        NativeAppsPartialPage nativeTab = appsPage.clickNativeAppsTab();

        //Select any listed native application.
        nativeTab.selectNativeApp(appName, true);

        //Verify the Edit button is enabled.
        Assert.assertFalse("Edit button is not enabled as expected when native application is selected.", nativeTab.isEditBtnDisabled());

        //Click the Edit button.
        EditNativeApplicationModal editModal = nativeTab.clickEdit();

        //Verify an Edit Native Application modal is displayed.
        Assert.assertTrue("Edit Native Application modal is not displayed as expected when Edit is clicked.", editModal.isEditNativeAppHeaderDisplayed());

        //Verify a ‘?’ icon and an ‘x’ icon are displayed in the top right corner of the modal.
        editModal.checkEditNativeAppHeaderIcons();

        //Click the ‘?’ icon.
        editModal.clickEditNativeAppHelpIcon();

        //Verify an Edit Native Application Help modal is displayed with details associated to editing a native application.
        editModal.checkHelpModal();

        //Verify an OK button is displayed.
        Assert.assertTrue("OK button is not displayed as expected in the Edit Native Application Help modal.", editModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        editModal.clickHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("Edit Native Application Help modal is still displayed after clicking OK.", editModal.isEditNativeAppHelpHeaderDisplayed());

        //Verify the following fields are displayed in the Edit Native Application modal: Name text box, Command Line text box, Host Address text box, and Port Number text box.
        editModal.checkAllFields();

        //Verify all fields are populated with the selected application’s data.
        editModal.checkAllFields(app);

        //Verify the following buttons are displayed at the bottom of the Edit Native Application modal: Update, Reset, and Cancel.
        editModal.checkBtns();

        //Get new values for native app
        String newName = RandomStringGenerator.nextLetterString(15);
        String newCommandLine = RandomStringGenerator.nextLetterString(5);
        String newHost = AppFactory.getRandomIP();
        String newPort = AppFactory.generateRandomValidPort();

        //Make a change to the application name value.
        editModal.setNativeAppName(newName);

        //Make a change to the application command line value.
        editModal.setCommandLine(newCommandLine);

        //Make a change to the application host address value.
        editModal.setHost(newHost);

        //Make a change to the application port number value.
        editModal.setPort(newPort);

        //Click the Reset button.
        editModal.clickReset();

        //Verify all values are reset to their original values.
        editModal.checkAllFields(app);

        //Make a change to all values
        editModal.setAllFields(newName, newCommandLine, newHost, newPort);

        //Click the Cancel button.
        editModal.clickCancel();

        //Verify the Edit Native Application modal is no longer displayed.
        Assert.assertFalse("Edit Native Application modal is still displayed after clicking Cancel.", editModal.isEditNativeAppHeaderDisplayed());

        //Verify the application is unchanged in the table of native applications.
        Assert.assertTrue("Original Native Application no longer displayed despite cancelling update.", nativeTab.isAppDisplayed(appName));

        //Verify the application is still selected, if not, select the application.
        if (nativeTab.isNativeAppRowSelected(app) == false) {
            nativeTab.selectNativeApp(appName, true);
        }

        //Click the Edit button.
        nativeTab.clickEdit();

        //Verify the Edit Native Application modal is displayed.
        Assert.assertTrue("Edit Native Application modal is not displayed as expected when Edit is clicked.", editModal.isEditNativeAppHeaderDisplayed());

        //Make a change to all values
        editModal.setAllFields(newName, newCommandLine, newHost, newPort);

        //Click the ‘x’ icon.
        editModal.clickCloseIcon();

        //Verify the Edit Native Application modal is no longer displayed.
        Assert.assertFalse("Edit Native Application modal is still displayed after clicking 'x' icon.", editModal.isEditNativeAppHeaderDisplayed());

        //Verify the application is unchanged in the table of native applications.
        Assert.assertTrue("Original Native Application no longer displayed despite closing the edit modal before updating.", nativeTab.isAppDisplayed(appName));

        //Verify the application is still selected, if not, select the application.
        if (nativeTab.isNativeAppRowSelected(app) == false) {
            nativeTab.selectNativeApp(appName, true);
        }

        //Click the Edit button.
        nativeTab.clickEdit();

        //Verify the Edit Native Application modal is displayed.
        Assert.assertTrue("Edit Native Application modal is not displayed as expected when Edit is clicked.", editModal.isEditNativeAppHeaderDisplayed());

        //Make a change to all values
        editModal.setAllFields(newName, newCommandLine, newHost, newPort);

        //Click the Update button.
        editModal.clickUpdate();

        //Verify the Edit Native Application modal is no longer displayed.
        Assert.assertFalse("Edit Native Application modal is still displayed after clicking Update.", editModal.isEditNativeAppHeaderDisplayed());

        //Verify the application is updated in the table of native applications and all updated values are displayed in the associated columns.
        Assert.assertFalse("Original Native Application still displayed despite succesfully updating the app.", nativeTab.isAppDisplayed(appName));
        Assert.assertTrue("Updated Native Application not displayed despite succesfully updating the app.", nativeTab.isAppDisplayed(newName));
        Assert.assertEquals("Native Application command line value not displayed as expected after the native application was updated.", newCommandLine,
                nativeTab.getNativeAppCellValue(newName, NativeAppColumn.CMD_LINE));
        Assert.assertEquals("Native Application host address value not displayed as expected after the native application was updated.", newHost,
                nativeTab.getNativeAppCellValue(newName, NativeAppColumn.HOST_ADDR));
        Assert.assertEquals("Native Application port value not displayed as expected after the native application was updated.", newPort,
                nativeTab.getNativeAppCellValue(newName, NativeAppColumn.PORT));

        //Verify the Device Type and ID of the native application remain unchanged.
        Assert.assertEquals("Native Application device type value was changed after the native application was updated.", app.getDeviceType().getUILabel(),
                nativeTab.getNativeAppCellValue(newName, NativeAppColumn.DEVICE_TYPE));
        Assert.assertEquals("Native Application ID value was changed after the native application was updated.", app.getID(), nativeTab.getNativeAppCellValue(newName, NativeAppColumn.ID));

        //Update entity manager
        UserNativeApp origApp = ETexasEntityManager.getNativeApp(appName);
        origApp.setName(newName);
        origApp.setCommandLine(newCommandLine);
        origApp.setHost(newHost);
        origApp.setPort(newPort);

        //Logout
        nativeTab.logout(testUser);
    }
}
