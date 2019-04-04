package com.harmonia.qa.ETEXASWebQATests.AppManagementTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.AppFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.apps.ETexasAppUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.CreateRemoteApplicationModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.EditRemoteApplicationModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.RemoteApplicationsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.RemoteApplicationsPartialPage.RemoteAppColumn;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Executes steps of Edit a Remote Application test, TC-089
 *
 * @author rsmith
 */
public class EditARemoteApplicationTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * User Remote App used throughout the test case
     */
    private UserRemoteApp app;

    /**
     * User Remote App used throughout the test case
     */
    private UserRemoteApp app2;

    /**
     * Remote App String name used throughout the test case
     */
    private String appName;

    /**
     * Remote App String name used for second app
     */
    private String appName2;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get test user and app entities
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        app = AppFactory.getUserRemoteApp(true); //Get a random remote app
        app2 = AppFactory.getUserRemoteApp(true); //Gets a random remote app
        appName = app.getName();
        appName2 = app2.getName();
        app.setUser(testUser);
        app2.setUser(testUser);
        ETexasEntityManager.addEntities(testUser, app, app2);

        //Register user and create remote app
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasAppUtils.createRemoteApp(app);
        ETexasAppUtils.createRemoteApp(app2);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-089
     */
    @Test
    public void editRemoteAppExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Click the Applications button in the top menu bar.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        AppsPage appsPage = simPage.clickApps();

        //Verify the Connected Vehicle Applications page is displayed.
        Assert.assertTrue("The Connected Vehicle Application page is not displayed as expected after clicking 'Applications'.", appsPage.isAppsHeaderDisplayed());

        //Verify the following tabs are displayed along the top of the page: Embedded Applications, JAR Applications, remote Applications, and Remote Applications.
        appsPage.checkAllTabs();

        //Click the Remote Applications tab.
        RemoteApplicationsPartialPage remoteAppsPage = appsPage.clickRemoteAppsTab();

        //Select any listed remote application
        remoteAppsPage.selectRemoteApp(app, true);

        //Verify the Edit button is enabled
        Assert.assertFalse("Edit button is not enabled as expected when remote application is selected.", remoteAppsPage.isEditBtnDisabled());

        //Click the Edit Button
        EditRemoteApplicationModal editModal = remoteAppsPage.clickEditBtn();

        //Verify an Edit Remote Application modal is displayed
        Assert.assertTrue("Edit Remote Application modal is not displayed as expected when Edit is clicked.", editModal.isEditRemoteAppHeaderDisplayed());

        //Verify a ‘?’ icon and an ‘x’ icon are displayed in the top right corner of the modal.
        editModal.checkEditRemoteAppHeaderIcons();

        //Click the '?' icon.
        editModal.clickEditRemoteAppHelpIcon();

        //Verify an Edit Remote Application Help modal is displayed with details associated to editing a remote application.
        editModal.checkHelpModal();

        //Verify an OK button is displayed.
        Assert.assertTrue("OK button is not displayed as expected in the Edit Remote Application Help modal.", editModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        editModal.clickHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("Edit Remote Application Help modal is still displayed after clicking OK.", editModal.isEditRemoteAppHelpHeaderDisplayed());

        //Verify a Name text box is displayed in the Edit Remote Application modal.
        editModal.checkFieldsDisplayed();

        //Verify the Name text box is populated with the name of the selected application.
        editModal.checkNameField(app);

        //Verify the following buttons are displayed at the bottom of the Edit Remote Application modal: Update, Reset, and Cancel.
        editModal.checkBtns();

        //Get new name value for remote app.
        String newName = RandomStringGenerator.nextLetterString(15);

        //Make a change to the application name value.
        editModal.setNameText(newName);

        //Click the Reset Button
        editModal.clickResetBtn();

        //Verify the name value is reset to its original value
        editModal.checkNameField(app);

        //Make a change to the application name value
        editModal.setNameText(newName);

        //Click the Cancel button.
        editModal.clickCancelBtn();

        //Verify the Edit Remote Application modal is no longer displayed
        Assert.assertFalse("Edit Remote Application modal is still displayed after clicking Cancel.", editModal.isEditRemoteAppHeaderDisplayed());

        //Verify the application is unchanged in the table of remote applications
        Assert.assertTrue("Original Remote Application no longer displayed despite cancelling update.", remoteAppsPage.isAppDisplayed(app));

        //Verify the application is still selected, if not, select the application.
        if (remoteAppsPage.isRemoteAppRowSelected(app) == false) {
            remoteAppsPage.selectRemoteApp(app, true);
        }

        //Click the Edit button.
        remoteAppsPage.clickEditBtn();

        //Verify the Edit Remote Application modal is displayed
        Assert.assertTrue("Edit Remote Application modal is not displayed as expected when Edit is clicked.", editModal.isEditRemoteAppHeaderDisplayed());

        //Make a change to the application name value
        editModal.setNameText(newName);

        //Click the 'x' icon.
        editModal.clickCloseIcon();

        //Verify the Edit Remote Application modal is no longer displayed
        Assert.assertFalse("Edit Remote Application modal is still displayed after clicking 'x' icon.", editModal.isEditRemoteAppHeaderDisplayed());

        //Verify the application is unchanged in the table of remote applications
        Assert.assertTrue("Original Remote Application no longer displayed after click 'x' icon.", remoteAppsPage.isAppDisplayed(app));

        //Verify the application is still selected, if not, select the application.
        if (remoteAppsPage.isRemoteAppRowSelected(app) == false) {
            remoteAppsPage.selectRemoteApp(app, true);
        }

        //Click the Edit button
        remoteAppsPage.clickEditBtn();

        //Verify the Edit Remote Application modal is displayed.
        Assert.assertTrue("Edit Remote Application modal is not displayed as expected when Edit is clicked.", editModal.isEditRemoteAppHeaderDisplayed());

        //Make a change to the application name value
        editModal.setNameText(newName);

        //Click the Update button.
        editModal.clickUpdate(true);

        //Verify the Edit Remote Application modal is no longer displayed.
        Assert.assertFalse("Edit Remote Application modal is still displayed after clicking Update.", editModal.isEditRemoteAppHeaderDisplayed());

        //Verify the application name is updated in the table of remote applications
        Assert.assertFalse("Original Remote Application still displayed despite succesfully updating the app.", remoteAppsPage.isAppDisplayed(app));
        Assert.assertTrue("Updated Remote Application not displayed despite succesfully updating the app.", remoteAppsPage.isAppDisplayed(newName));

        //Verify the Device Type and ID of the remote application remain unchanged.
        Assert.assertEquals("Remote Application device type value was changed after the remote application was updated.", app.getDeviceType().getUILabel(),
                remoteAppsPage.getRemoteAppCellValue(newName, RemoteAppColumn.DEVICE_TYPE));
        Assert.assertEquals("Remote Application ID value was changed after the remote application was updated.", app.getID(), remoteAppsPage.getRemoteAppCellValue(newName, RemoteAppColumn.ID));

        //Update entity manager
        UserRemoteApp origApp = ETexasEntityManager.getRemoteApp(appName);
        origApp.setName(newName);

        //Log out
        remoteAppsPage.logout(testUser);
    }

    /**
     * Test steps for ITC-066
     */
    @Test
    public void editRemoteAppInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Click the Applications button in the top menu bar.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        AppsPage appsPage = simPage.clickApps();

        //Verify the Connected Vehicle Applications page is displayed.
        Assert.assertTrue("The Connected Vehicle Application page is not displayed as expected after clicking 'Applications'.", appsPage.isAppsHeaderDisplayed());

        //Click the Remote Applications tab.
        RemoteApplicationsPartialPage remoteAppsPage = appsPage.clickRemoteAppsTab();

        //Select any listed remote application
        remoteAppsPage.selectRemoteApp(app, true);

        //Verify the Edit button is enabled
        Assert.assertFalse("Edit button is not enabled as expected when remote application is selected.", remoteAppsPage.isEditBtnDisabled());

        //Click the Edit Button
        EditRemoteApplicationModal editModal = remoteAppsPage.clickEditBtn();

        //Delete the pre-populated value in the Name text box.
        editModal.setNameText("");

        //Click the Update button
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Name text box indicating a valid name is required.
        editModal.checkRequiredFieldErrorAllFields();

        //Enter one or more space characters in the Name textbox
        editModal.setNameText("  ");
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Name text box indicating a valid name is required.
        Assert.assertTrue("Error is not displayed as expected when space characters are entered in Application Name field", editModal.isInvalidAppNameErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes
        editModal.setNameText(RandomStringGenerator.nextString(10, true));
        editModal.clickUpdate(false);

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Name Error is not displayed as expected when special characters are entered in Application Name field", editModal.isInvalidNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text.
        editModal.setNameText(RandomStringGenerator.nextString(10, false) + "   " + (RandomStringGenerator.nextString(5, false)));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Name Error is not displayed as expected when consecutive spaces are entered in Application Name field", editModal.isInvalidNameErrorDisplayed());

        //Enter a value enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        editModal.setNameText(" " + (RandomStringGenerator.nextString(10, false)));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Error is not displayed as expected when the value entered begins with a space in the Application Name field", editModal.isNameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’)
        editModal.setNameText(RandomStringGenerator.nextString(10, false) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Error is not displayed as expected when the value entered ends with a space in the Application Name field", editModal.isNameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g. '-Test')
        editModal.setNameText("-" + (RandomStringGenerator.nextString(10, false)));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Name Error is not displayed as expected when the value entered begins with a hyphen/dash in the Application Name field", editModal.isInvalidNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g. 'Test-')
        editModal.setNameText(RandomStringGenerator.nextString(10, false) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Name Error is not displayed as expected when the value entered ends with a hyphen/dash in the Application Name field", editModal.isInvalidNameErrorDisplayed());

        //Enter a valid name in the Name text box.
        String newName = RandomStringGenerator.nextString(10, false);
        editModal.setNameText(newName);

        //Click the Update Button
        editModal.clickUpdate(true);

        //Verify the new application is displayed in the Applications list
        Assert.assertTrue("New Remote Application is not displayed in the Applications list after update.", remoteAppsPage.isAppDisplayed(newName));

        // Click the create button
        CreateRemoteApplicationModal createModal = remoteAppsPage.clickCreate();

        //Enter the name of an existing application in the Name text box.
        createModal.setNameText(appName2);

        //Verify an error is displayed associated with the Name text box indicating the field requires a unique value.
        Assert.assertTrue("Error is not displayed as expected indicating that field requires a unique value", createModal.isDuplicateAppNameErrorDisplayed(app2));

        //Click the Cancel Button
        createModal.clickCancelBtn();

        //Update entity manager
        UserRemoteApp origApp = ETexasEntityManager.getRemoteApp(appName);
        origApp.setName(newName);

        //Log out
        remoteAppsPage.logout(testUser);
    }
}
