package com.harmonia.qa.ETEXASWebQATests.AppManagementTests;

import java.util.ArrayList;
import java.util.List;

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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.RemoteApplicationsPartialPage;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Create a Remote Application test,
 * TC-043, ITC-030
 *
 * @author llaroussini
 * @author rsmith
 */
public class CreateARemoteApplicationTest extends ETexasAfterTestResetTestBase {

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
        List<UserRemoteApp> apps = new ArrayList<UserRemoteApp>(2);
        apps.add(app);
        apps.add(app2);
        testUser.setRemoteApps(apps);
        ETexasEntityManager.addEntities(testUser, app, app2);

        //Register user and create remote app for ITC
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasAppUtils.createRemoteApp(app2);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-043
     */
    @Test
    public void createRemoteAppExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Click the Applications button in the top menu bar.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        AppsPage appsPage = simPage.clickApps();

        //Verify the Connected Vehicle Applications page is displayed.
        Assert.assertTrue("The Connected Vehicle Application page is not displayed as expected after clicking 'Applications'.", appsPage.isAppsHeaderDisplayed());

        //Verify the following tabs are displayed along the top of the page: Embedded Applications, JAR Applications, Native Applications, and Remote Applications.
        appsPage.checkAllTabs();

        //Click the Remote Applications tab.
        RemoteApplicationsPartialPage remoteAppsPage = appsPage.clickRemoteAppsTab();

        //Verify the following buttons are displayed: Create, Edit, Delete, and Parameters.
        remoteAppsPage.checkBtnsDisplayed();

        //Verify the Create button is enabled by default.
        Assert.assertFalse("Create button is not enabled as expected when Remote Apps tab is first loaded.", remoteAppsPage.isCreateBtnDisabled());

        //Verify the Edit, Delete, and Parameter buttons are disabled by default.
        Assert.assertTrue("Edit button is not disabled as expected when Remote Apps tab is first loaded.", remoteAppsPage.isEditBtnDisabled());
        Assert.assertTrue("Delete button is not disabled as expected when Remote Apps tab is first loaded.", remoteAppsPage.isDeleteBtnDisabled());
        Assert.assertTrue("Parameter button is not disabled as expected when Remote Apps tab is first loaded.", remoteAppsPage.isParametersBtnDisabled());

        //Verify a table is displayed with existing remote applications, if any exist.
        int appListSize = testUser.getRemoteApps().size();
        if (appListSize == 0) {
            Assert.assertFalse("List of remote apps displayed despite user not having added any remote apps.", remoteAppsPage.areRemoteAppsDisplayed());
        }
        else {
            Assert.assertTrue("List of remote apps not displayed despite user having previously added remote apps.", remoteAppsPage.areRemoteAppsDisplayed());
        }

        //Verify the following columns display: ID, Name, and Device Type.
        remoteAppsPage.checkColumnHeaderCells();

        //Click the Create button.
        CreateRemoteApplicationModal createModal = remoteAppsPage.clickCreate();

        //Verify a Create Remote Application modal is displayed.
        Assert.assertTrue("Create Remote Application header not displayed after clicking Create.", createModal.isHeaderDisplayed());

        //Verify a ‘?’ icon and an ‘x’ icon are displayed in the top right corner of the modal.
        createModal.checkHeaderIcons();

        //Click the ‘?’ icon.
        createModal.clickHelpIcon();

        //Verify a Create Remote Application Help modal is displayed with instructions for creating a remote application.
        createModal.checkHelpModal();

        //Verify an OK button is displayed and click the OK button.
        Assert.assertTrue("The OK button is not displayed on the Create Remote Application Help window.", createModal.isHelpOKBtnDisplayed());
        createModal.clickHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("The help text is still displayed after clicking OK in the Create Remote Application Help window.", createModal.isHelpContentDisplayed());

        //Verify the following fields are displayed in the Create Remote Application modal: Name text box and Device Type dropdown.
        createModal.checkFieldsDisplayed();

        //Verify the following buttons are displayed at the bottom of the Create Remote Application modal: Create, Reset, and Cancel.
        createModal.checkFormBtns();

        //Enter a valid value in the Name text box.
        createModal.setNameText(app);

        //Select a type from the App Type dropdown.
        createModal.selectDeviceType(app);

        //Click the Reset button.
        createModal.clickResetBtn();

        //Verify all values are returned to their default state.
        createModal.checkDefaultValues();

        //Enter valid values in all fields.
        createModal.setAllFields(app);

        //Click the Cancel button.
        createModal.clickCancelBtn();

        //Verify the Create Remote Application modal is no longer displayed.
        Assert.assertFalse("Register Remote App modal is still displayed after clicking the Cancel button.", createModal.isHeaderDisplayed());

        //Verify the application is not displayed in the table of remote applications.
        Assert.assertFalse("App with name, " + app.getName() + ", displays in Remote Apps table after clicking cancel.", remoteAppsPage.isAppDisplayed(app));

        //Click the Create button.
        remoteAppsPage.clickCreate();

        //Verify all fields display default values.
        createModal.checkDefaultValues();

        //Enter valid values in all fields.
        createModal.setAllFields(app);

        //Click the ‘x’ icon.
        createModal.clickCloseIcon();

        //Verify the Create Remote Application modal is no longer displayed.
        Assert.assertFalse("Register Remote App modal is still displayed after clicking the Close icon.", createModal.isHeaderDisplayed());

        //Verify the application is not displayed in the table of remote applications.
        Assert.assertFalse("App with name, " + app.getName() + ", displays in Remote Apps table after clicking close.", remoteAppsPage.isAppDisplayed(app));

        //Click the Create button.
        remoteAppsPage.clickCreate();

        //Verify default values are displayed in all fields.
        createModal.checkDefaultValues();

        //Enter valid values in all fields.
        createModal.setAllFields(app);

        //Click the Create button.
        createModal.clickCreate(true);

        //Verify the application is displayed in the table of remote applications.
        Assert.assertTrue("App with name, " + app.getName() + " could not be found in Remote Apps table after clicking the Create button.", remoteAppsPage.isAppDisplayed(app));

        //Verify the Name and Device Type columns are populated with the values entered previously.
        Assert.assertTrue("New remote app name is not displayed despite successful creation.", remoteAppsPage.isAppNameDisplayed(app.getName()));
        Assert.assertEquals("The value in the Type column for the newly created remote app is not displayed as expected.", app.getDeviceType().getUILabel(), remoteAppsPage.getDeviceType(app));

        //Verify the newly created application has an auto-generated ID displayed in the ID column.
        Assert.assertNotNull("An ID was not assigned to the new remote app as expected.", remoteAppsPage.getRemoteAppID(app));

        //Verify the newly created application is selected.
        Assert.assertTrue("Newly created app row not selected as expected.", remoteAppsPage.isRemoteAppRowSelected(app));

        //Verify the Create, Edit, Delete, and Parameter buttons are all enabled.
        Assert.assertFalse("Create button is not enabled as expected when remote app is selected.", remoteAppsPage.isCreateBtnDisabled());
        Assert.assertFalse("Edit button is not enabled as expected when remote app is selected.", remoteAppsPage.isEditBtnDisabled());
        Assert.assertFalse("Delete button is not enabled as expected when remote app is selected.", remoteAppsPage.isEditBtnDisabled());
        Assert.assertFalse("App Parameters button is not enabled as expected when remote app is selected.", remoteAppsPage.isParametersBtnDisabled());

        //Log out
        remoteAppsPage.logout(testUser);
    }

    /**
     * Test steps for ITC-030
     */
    @Test
    public void createRemoteAppInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Click the Applications button in the top menu bar.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        AppsPage appsPage = simPage.clickApps();

        //Verify the Connected Vehicle Applications page is displayed.
        Assert.assertTrue("The Connected Vehicle Application page is not displayed as expected after clicking 'Applications'.", appsPage.isAppsHeaderDisplayed());

        //Click the Remote Applications tab.
        RemoteApplicationsPartialPage remoteAppsPage = appsPage.clickRemoteAppsTab();

        //Click the Create button
        CreateRemoteApplicationModal createModal = remoteAppsPage.clickCreate();

        //With all fields blank, click the Create button.
        createModal.clickCreate(false);

        //Verify field required errors are displayed with all fields
        createModal.checkRequiredFieldErrorAllFields();

        //Enter one or more space characters in the Name textbox, select a device from dropdown, click create
        createModal.setNameText("  ");
        createModal.selectDeviceType(app);
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Name text box indicating a valid name is required.
        Assert.assertTrue("Error is not displayed as expected when space characters are entered in Application Name field", createModal.isInvalidAppNameErrorDisplayed());

        //Enter a value in the Name text box that contains special characters, not including dashes
        createModal.setNameText(RandomStringGenerator.nextLetterString(5) + "*$()$*($#");
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Name Error is not displayed as expected when special characters are entered in Application Name field", createModal.isInvalidNameErrorDisplayed());

        //Enter a value in the Name text box that contains consecutive spaces within the entered text.
        createModal.setNameText(RandomStringGenerator.nextString(10, false) + "   " + (RandomStringGenerator.nextString(5, false)));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Name Error is not displayed as expected when consecutive spaces are entered in Application Name field", createModal.isInvalidNameErrorDisplayed());

        //Enter a value enter a value in the Name text box that begins with a space (e.g., ‘ Test’).
        createModal.setNameText(" " + (RandomStringGenerator.nextString(10, false)));

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Error is not displayed as expected when the value entered begins with a space in the Application Name field", createModal.isNameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that ends with a space (e.g., ‘Test ’)
        createModal.setNameText(RandomStringGenerator.nextString(10, false) + " ");

        //Verify an error is displayed associated with the Name text box indicating the field does not support leading/trailing whitespace.
        Assert.assertTrue("Error is not displayed as expected when the value entered ends with a space in the Application Name field", createModal.isNameWhitespaceErrorDisplayed());

        //Enter a value in the Name text box that begins with a hyphen/dash (e.g. '-Test')
        createModal.setNameText("-" + (RandomStringGenerator.nextString(10, false)));

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Name Error is not displayed as expected when the value entered begins with a hyphen/dash in the Application Name field", createModal.isInvalidNameErrorDisplayed());

        //Enter a value in the Name text box that ends with a hyphen/dash (e.g. 'Test-')
        createModal.setNameText(RandomStringGenerator.nextString(10, false) + "-");

        //Verify an error is displayed associated with the Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Name Error is not displayed as expected when the value entered ends with a hyphen/dash in the Application Name field", createModal.isInvalidNameErrorDisplayed());

        //Enter a valid name in the Name text box.
        String newName = RandomStringGenerator.nextString(10, false);
        createModal.setNameText(newName);

        //Click the Create Button
        createModal.clickCreate(true);

        //Verify the new application is displayed in the Applications list
        Assert.assertTrue("New Remote Application is not displayed in the Applications list after Create.", remoteAppsPage.isAppDisplayed(newName));

        //Click the Create button.
        remoteAppsPage.clickCreate();

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
