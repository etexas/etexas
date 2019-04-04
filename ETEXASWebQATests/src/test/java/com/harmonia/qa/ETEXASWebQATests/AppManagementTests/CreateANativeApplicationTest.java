package com.harmonia.qa.ETEXASWebQATests.AppManagementTests;

import java.util.ArrayList;
import java.util.List;

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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.CreateNativeApplicationModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.NativeAppsPartialPage;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomNumberGenerator;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Manage User Native Apps test, TC-042
 *
 * @author llaroussini
 * @author rsmith
 */
public class CreateANativeApplicationTest extends ETexasAfterTestResetTestBase {

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
     * Second Native App used throughout the test case
     */
    private UserNativeApp app2;

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

        //Get test user and complete registration
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);

        //Create user app entity
        app = AppFactory.getUserNativeApp(true); //Get a random native app
        app2 = AppFactory.getUserNativeApp(true);
        appName = app.getName();
        appName2 = app2.getName();
        app.setUser(testUser);
        app2.setUser(testUser);
        List<UserNativeApp> apps = new ArrayList<UserNativeApp>(1);
        apps.add(app2);
        testUser.setNativeApps(apps);

        //Add entities to entity manager
        ETexasEntityManager.addEntities(testUser, app, app2);
        ETexasAppUtils.createNativeApp(app2); //creates second native app

        //User is logged in and on Apps Page
        SimulationsPage simPage = landing.loginAs(testUser);
        simPage.clickApps();
    }

    /**
     * Test steps for TC-042: Create a Native Application
     */
    @Test
    public void createNativeApplicationExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Ensure the Apps page is loaded
        AppsPage appsPage = getPage(AppsPage.class);
        Assert.assertTrue("Apps header is not displayed as expected.", appsPage.isAppsHeaderDisplayed());

        //Verify all expected tabs display
        appsPage.checkAllTabs();

        //Click the Native Apps tab, then verify Create, Edit, Delete, and Parameters buttons are visible.
        NativeAppsPartialPage nativeAppsPage = appsPage.clickNativeAppsTab();
        nativeAppsPage.checkBtnsDisplayed();

        //Verifies that Create button is enabled, and verifies that Edit, Delete, and Parameters buttons are disabled.
        Assert.assertFalse("Create button is not enabled as expected when User Native Apps tab is first loaded.", nativeAppsPage.isCreateBtnDisabled());
        Assert.assertTrue("Edit button is not disabled as expected when User Native Apps tab is first loaded.", nativeAppsPage.isEditBtnDisabled());
        Assert.assertTrue("Delete button is not disabled as expected when User Native Apps tab is first loaded.", nativeAppsPage.isDeleteBtnDisabled());
        Assert.assertTrue("Parameters button is not disabled as expected when User Native Apps tab is first loaded.", nativeAppsPage.isParametersBtnDisabled());

        //Verify a table displays with existing applications, if any exist
        int appListSize = testUser.getNativeApps().size();
        if (appListSize == 0) {
            Assert.assertFalse("List of native apps displayed despite user not having added any native apps.", nativeAppsPage.areNativeAppsDisplayed());
        }
        else {
            Assert.assertTrue("List of native apps not displayed despite user having previously added native apps.", nativeAppsPage.areNativeAppsDisplayed());
        }

        //Verify ID, Name, Device Type, Command Line, Host Address, and Port columns display
        nativeAppsPage.checkColumnHeaderCells();

        //Click Create and verify Create Native Application window displays
        CreateNativeApplicationModal registerForm = nativeAppsPage.clickCreate();
        Assert.assertTrue("Create Native Application modal not displayed after clicking Add.", registerForm.isCreateNativeAppHeaderDisplayed());

        //Verify Create Native Application help and close icons display in header
        registerForm.checkCreateNativeAppHeaderIcons();

        //Click the Create Native Application help icon, verify help window displays
        registerForm.clickCreateNativeAppHelpIcon();
        Assert.assertTrue("Create Native Application Help modal header not displayed after clicking help icon.", registerForm.isCreateNativeAppHelpHeaderDisplayed());
        Assert.assertTrue("Create Native Application Help content could not be found after clicking help icon.", registerForm.isHelpContentDisplayed());

        //Verify OK button displays, click OK, verify help window closes
        Assert.assertTrue("OK button could not be found in Create Native Application Help modal.", registerForm.isHelpOKBtnDisplayed());
        registerForm.clickHelpOKBtn();
        Assert.assertFalse("Create Native Application Help modal header still displayed after clicking OK button.", registerForm.isCreateNativeAppHelpHeaderDisplayed());

        //Verify expected fields display in create native app modal (Name text box, Device Type dropdown, Command Line text box, Host Address text box, and Port Number text box)
        registerForm.checkAllFields();

        //Verify expected buttons (Create, Reset, Cancel) display
        registerForm.checkBtns();

        //Enter values in all fields, click Reset, verify values are cleared
        registerForm.setAllFields(app);
        registerForm.checkAllFields(app);
        registerForm.clickReset();
        registerForm.checkClearedFields();

        //Enter values in all fields, click Cancel, verify app is NOT listed
        registerForm.setAllFields(app);
        registerForm.clickCancel();
        Assert.assertFalse("Create Native Application modal is still displayed after clicking cancel.", registerForm.isCreateNativeAppHeaderDisplayed());
        Assert.assertFalse("App with name, " + appName + ", displays in User Native Apps table after clicking cancel.", nativeAppsPage.isAppDisplayed(app));

        //Click Create, check default values, enter valid values in all fields, and check all fields for valid input
        nativeAppsPage.clickCreate();
        registerForm.checkClearedFields();
        registerForm.setAllFields(app);
        registerForm.checkAllFields();

        //Click 'x' icon and verify Create Native Application form closes, then verifies no app was created after clicking x icon
        registerForm.clickCloseIcon();
        Assert.assertFalse("Create Native Application model is still displayed after selecting the 'x' icon", registerForm.isCreateNativeAppHeaderDisplayed());
        Assert.assertFalse("App with name, " + appName + ", displays in User Native Apps table after clicking the 'x' icon.", nativeAppsPage.isAppDisplayed(app));

        //Creates app with valid values
        nativeAppsPage.clickCreate();
        registerForm.checkClearedFields();
        registerForm.setAllFields(app);
        registerForm.checkAllFields();
        registerForm.clickCreate();

        //Verify app displays in list of apps
        Assert.assertTrue("App with name, " + appName + " could not be found in User Native Apps table after clicking register app.", nativeAppsPage.isAppDisplayed(app));

        //Verify values in table match values entered previously
        Assert.assertTrue("App Name in table does not match expected value.", nativeAppsPage.isAppDisplayed(appName));
        Assert.assertTrue("Device Type in table does not match expected value", nativeAppsPage.isAppCellDisplayed(appName, app.getDeviceType().getUILabel()));
        Assert.assertTrue("Command Line values in table do not match expected value", nativeAppsPage.isAppCellDisplayed(appName, app.getCommandLine()));
        Assert.assertTrue("Host Address in table do not match expected value", nativeAppsPage.isAppCellDisplayed(appName, app.getHost()));
        Assert.assertTrue("Port in table does not match expected value.", nativeAppsPage.isAppCellDisplayed(appName, app.getPort()));
        Assert.assertTrue("An ID was not assigned to the application row.", nativeAppsPage.isAppIDSet(appName));

        //Selects Application and verifies Create, Edit, Delete, and Parameter buttons are enabled
        nativeAppsPage.selectNativeApp(app, true);
        Assert.assertFalse("Create button is not enabled when a simulation is selected.", nativeAppsPage.isCreateBtnDisabled());
        Assert.assertFalse("Edit button is not enabled when a simulation is selected.", nativeAppsPage.isEditBtnDisabled());
        Assert.assertFalse("Delete button is not enabled when a simulation is selected.", nativeAppsPage.isDeleteBtnDisabled());
        Assert.assertFalse("Parameter button is not enabled when a simulation is selected.", nativeAppsPage.isParametersBtnDisabled());

        //Log out
        nativeAppsPage.logout(testUser);
    }

    /**
     * Test steps for ITC-029
     */
    @Test
    public void createNativeApplicationInternalTest() {

        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Ensure the Apps page is loaded
        AppsPage appsPage = getPage(AppsPage.class);
        Assert.assertTrue("Apps header is not displayed as expected.", appsPage.isAppsHeaderDisplayed());

        //Verify all expected tabs display
        appsPage.checkAllTabs();

        //Click the Native Apps tab, then verify Create, Edit, Delete, and Parameters buttons are visible.
        NativeAppsPartialPage nativeAppsPage = appsPage.clickNativeAppsTab();

        //Click Create and verify Create Native Application window displays
        CreateNativeApplicationModal createModal = nativeAppsPage.clickCreate();
        Assert.assertTrue("Create Native Application modal not displayed after clicking Create.", createModal.isCreateNativeAppHeaderDisplayed());

        //With all fields blank, click the Create button.
        createModal.clickCreate(false);

        //Verify all fields display error indicating fields are required
        createModal.checkRequiredFieldErrorAllFields();

        //Enter one or more space characters in the Native App Name text box, enter valid values in the remaining fields, and click the Create button.
        createModal.setAllFields(app);
        createModal.setNativeAppName("  ");
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Native App Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Error is not displayed as expected when space characters are entered in Application Name field", createModal.isInvalidAppNameErrorDisplayed());

        //Enter a value in the Native App Name text box that contains special characters, not including dashes (e.g., ‘T3$t’).amd click Create
        createModal.setNativeAppName(RandomStringGenerator.nextLetterString(5) + "*$()$*($#");
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Native App Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid App Name error is not displayed as expected when App Name text box contains special characters.", createModal.isInvalidNameErrorDisplayed());

        //Enter a value in the Native App Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’). and click Create
        createModal.setNativeAppName(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Native App Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid App Name error is not displayed as expected when App Name text box contains consecutive spaces.", createModal.isInvalidNameErrorDisplayed());

        //Enter a value in the Native App Name text box that begins with a space (e.g., ‘ Test’). and click Create
        createModal.setNativeAppName(" " + RandomStringGenerator.nextLetterString(5));
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Native App Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid App Name error is not displayed as expected when App Name text box contains a leading space.", createModal.isAppNameLeadingTrailingWhiteSpaceErrorDisplayed());

        //Enter a value in the Native App Name text box that ends with a space (e.g., ‘Test ’). and click Create
        createModal.setNativeAppName(RandomStringGenerator.nextLetterString(5) + " ");
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Native App Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid App Name error is not displayed as expected when App Name text box contains a trailing space.", createModal.isAppNameLeadingTrailingWhiteSpaceErrorDisplayed());

        //Enter a valid name in the Native App Name text box and delete the text from the Command Line text box and enter one or more space characters. And click Create
        String newName = RandomStringGenerator.nextString(10, false);
        createModal.setNativeAppName(newName);
        createModal.setCommandLine("  ");
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Command Line text box indicating a valid Command Line is required.
        Assert.assertTrue("Invalid Command Line error is not displayed as expected when Command Line text box contains whitespace only.", createModal.isBlankCommandLineErrorDisplayed());

        //Enter a value in the Command Line text box that contains special characters, not including hyphens, periods, or underscores (e.g., ‘T3$t’). And click Create
        createModal.setCommandLine(RandomStringGenerator.nextString(10, true));
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Command Line text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Command Line error is not displayed as expected when Command Line text box contains special characters.", createModal.isInvalidCommandLineErrorDisplayed());

        //Enter a value in the Command Line text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’). And click Create
        createModal.setCommandLine(RandomStringGenerator.nextLetterString(5) + "  " + RandomStringGenerator.nextLetterString(5));
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Command Line text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Command Line error is not displayed as expected when Command Line text box contains consecutive spaces.", createModal.isInvalidCommandLineErrorDisplayed());

        //Enter a value in the Command Line text box that begins with a space (e.g., ‘ Test’). And click create
        createModal.setCommandLine(" " + RandomStringGenerator.nextLetterString(5));
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Command Line text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Command Line error is not displayed as expected when Command Line text box contains a leading space.",
                createModal.isCommandLineLeadingTrailingWhiteSpaceErrorDisplayed());

        //Enter a value in the Command Line text box that ends with a space (e.g., ‘Test ’). And click Create.
        createModal.setCommandLine(RandomStringGenerator.nextLetterString(5) + " ");
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Command Line text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Command Line error is not displayed as expected when Command Line text box contains a trailing space.",
                createModal.isCommandLineLeadingTrailingWhiteSpaceErrorDisplayed());

        //Enter a valid value in the Command Line text box, delete the text from the Host text box and enter one or more space characters and click Create
        createModal.setCommandLine(app);
        createModal.setHost("  ");
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Host text box indicating a valid Host Address is required.
        Assert.assertTrue("Invalid Host error is not displayed as expected when Host text box contains whitespace only.", createModal.isBlankHostAddressErrorDisplayed());

        //Enter a random string of characters in the Host text box and Click Create
        String randomHost = (RandomStringGenerator.nextLetterString(10));
        createModal.setHost(randomHost);
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Host text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Host error is not displayed as expected when Host text box contains random string.", createModal.isInvalidHostAddressErrorDisplayed(randomHost));

        //Enter a valid value in the Host text box and enter a non-numeric value in the Port textbox
        createModal.setHost(app);
        String invalidPort = ("...");
        createModal.setPort(invalidPort);

        //Verify an error icon associated with the Port text box indicating the field does not support non-numerical values.
        Assert.assertTrue("alid (non-numeric) Port error is not displayed as expected when Port text box contains a non-numeric value.",
                createModal.isInvalidNonNumericPortErrorDisplayed(invalidPort));

        //Enter a value greater than 65535 in the Port text box and Click Create
        createModal.setPort(Integer.toString(RandomNumberGenerator.nextInteger(1000) + 65536));
        createModal.clickCreate(false);

        //Verify an error icon associated with the Port text box indicating the maximum value for the field is 65535.
        Assert.assertTrue("Invalid Port error, max exceeded, is not displayed as expected when Port text box contains value over 65535.", createModal.isPortMaxExceededErrorDisplayed());

        //Enter a valid value in the Port text box and click Create
        createModal.setPort(app);
        createModal.clickCreate(true);

        //Verify the new app is displayed in the Apps list.
        Assert.assertTrue("App with name, " + newName + " could not be found in User Native Apps table after clicking register app.", nativeAppsPage.isAppDisplayed(newName));

        //Click the create button.
        createModal = nativeAppsPage.clickCreate();

        //Enter the same app details (including the same name) and click Create
        createModal.setAllFields(app2);
        createModal.clickCreate(false);

        //Verify an error is displayed associated with the Native App Name text box indicating the field requires a unique value.
        Assert.assertTrue("Duplicate App Name error is not displayed as expected when App Name text box contains non-unique value.", createModal.isDuplicateAppNameErrorDisplayed(app2));

        //Click the Cancel button.
        createModal.clickCancel();

        //Update entity manager
        UserNativeApp origApp = ETexasEntityManager.getNativeApp(appName);
        origApp.setName(newName);

        //Logout
        nativeAppsPage.logout(testUser);
    }
}
