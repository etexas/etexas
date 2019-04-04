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
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.CreateJARApplicationModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.JARApplicationsPartialPage;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the Create a JAR of Applications test,
 * TC-044
 *
 * @author llaroussini
 * @author saistrop
 */
public class CreateAJAROfApplicationsTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Jar app used in test case
     */
    private UserJarApp jarApp;

    /**
     * Jar app used in internal test case
     */
    private UserJarApp itcJarApp;

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
     * The zip file used in internal test
     */
    private File zipFile;

    /**
     * File name of the zip to use in internal test
     */
    private String zipFileName = "testtempclass1.zip";

    /**
     * The invalid jar file used in internal test
     */
    private File invalidJarFile;

    /**
     * File name of the invalid jar to use in internal test
     */
    private String invalidJarFileName = "mysql-connector-java-5.1.29-bin.jar";

    /**
     * File path to access JAR file
     */
    private String applicationFilePath = "src/test/resources/";

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
        ETexasEntityManager.addEntities(testUser);

        //An application jar file exists
        uploadedAppFile = FileUtils.getFile(applicationFilePath, testFileName);

        //A zip file and invalid jar file exist for internal test case
        zipFile = FileUtils.getFile(applicationFilePath, zipFileName);
        invalidJarFile = FileUtils.getFile(applicationFilePath, invalidJarFileName);

        //User Jar App exists
        jarApp = AppFactory.getUserJarApp(RandomStringGenerator.nextLetterString(10), DeviceType.OBU, uploadedAppFile);
        jarApp.setUser(testUser);
        //Create additional jar app for internal test
        itcJarApp = AppFactory.getUserJarApp(RandomStringGenerator.nextLetterString(10), DeviceType.OBU, uploadedAppFile);
        itcJarApp.setUser(testUser);

        //Add entities to entity manager
        ETexasEntityManager.addEntities(testUser, jarApp, itcJarApp);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-044
     *
     * @throws IOException if errors are encountered reading the file name/path
     *         - likely an issue with file permissions
     */
    @Test
    public void createJarAppTestExternal() throws IOException {
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

        //Verify the following buttons are displayed: Create, Edit, Delete, and Parameters.
        jarAppsPage.checkBtnsDisplayed();

        //Verify the Create button is enabled by default.
        Assert.assertFalse("Create button is not enabled as expected when JAR Applications tab is first loaded.", jarAppsPage.isCreateBtnDisabled());

        //Verify the Edit, Delete, and Parameter buttons are disabled by default.
        Assert.assertTrue("Edit button is not disabled as expected when JAR Applications tab is first loaded.", jarAppsPage.isEditBtnDisabled());
        Assert.assertTrue("Delete button is not disabled as expected when JAR Applications tab is first loaded.", jarAppsPage.isDeleteBtnDisabled());
        Assert.assertTrue("Parameter button is not disabled as expected when JAR Applications tab is first loaded.", jarAppsPage.isParametersBtnDisabled());

        //Verify a table is displayed with existing application JARs, if any exist.
        int appListSize = testUser.getJarApps().size();
        if (appListSize == 0) {
            Assert.assertFalse("List of JAR apps displayed despite user not having added any JAR apps.", jarAppsPage.areJARAppsDisplayed());
        }
        else {
            Assert.assertTrue("List of JAR apps not displayed despite user having previously added JAR apps.", jarAppsPage.areJARAppsDisplayed());
        }

        //Verify the following columns display: ID, Name, and Device Type.
        jarAppsPage.checkColumnHeaderCells();

        //Click the Create button.
        CreateJARApplicationModal createModal = jarAppsPage.clickCreate();

        //Verify a Create Applications from JAR modal is displayed.
        Assert.assertTrue("Create Applications from JAR header not displayed after clicking Create.", createModal.isHeaderDisplayed());

        //Verify a ‘?’ icon and an ‘x’ icon are displayed in the top right corner of the modal.
        createModal.checkHeaderIcons();

        //Click the ‘?’ icon.
        createModal.clickHelpIcon();

        //Verify a Create Applications from JAR Help modal is displayed with instructions for creating a remote application.
        createModal.checkHelpModal();

        //Verify an OK button is displayed and click the OK button.
        Assert.assertTrue("The OK button is not displayed on the Create Applications from JAR Help window.", createModal.isHelpOKBtnDisplayed());
        createModal.clickHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("The help text is still displayed after clicking OK in the Create Applications from JAR Help window.", createModal.isHelpContentDisplayed());

        //Verify the following fields are displayed in the Create Applications from JAR modal: JAR Name text box and Upload selector with Browse button.
        createModal.checkFieldsDisplayed();

        //Verify the following buttons are displayed at the bottom of the Create Applications from JAR modal: Create, Reset, and Cancel.
        createModal.checkFormBtns();

        //Enter a valid value in the JAR Name text box.
        createModal.setJARName(jarApp);

        //Click the Browse button. (omitted due to the nature of Selenium interaction - we set the upload file directly rather than using the 'upload'/file browser dialog)
        //Verify a File dialog is displayed.
        //Select a JAR file.
        createModal.setUploadFile(uploadedAppFile);

        //Verify the JAR file path is populated in the Upload field. (Omitted - there is no good way to check this as neither the 'value' or text of the field actually update as far as Selenium is concerned)

        //Click the Reset button.
        createModal.clickResetBtn();

        //Verify all values are returned to their default state.
        createModal.checkResetValues();

        //Enter a valid value in the JAR Name text box.
        createModal.setJARName(jarApp);

        //Select a valid JAR file using the Browse button. (omitted due to the nature of Selenium interaction - we set the upload file directly rather than using the 'upload'/file browser dialog)
        createModal.setUploadFile(uploadedAppFile);

        //Click the Cancel button.
        createModal.clickCancelBtn();

        //Verify the Create Applications from JAR modal is no longer displayed.
        Assert.assertFalse("Create Applications from JAR modal is still displayed after clicking the Cancel button.", createModal.isHeaderDisplayed());

        //Verify the application is not displayed in the table of application JARs.
        Assert.assertFalse("App with name, " + jarApp.getName() + ", displays in JAR Apps table after clicking cancel.", jarAppsPage.isAppDisplayed(jarApp));

        //Click the Create button.
        jarAppsPage.clickCreate();

        //Verify all fields display default values.
        createModal.checkResetValues();

        //Enter a valid value in the JAR Name text box.
        createModal.setJARName(jarApp);

        //Select a valid JAR file using the Browse button. (omitted due to the nature of Selenium interaction - we set the upload file directly rather than using the 'upload'/file browser dialog)
        createModal.setUploadFile(uploadedAppFile);

        //Click the ‘x’ icon.
        createModal.clickCloseIcon();

        //Verify the Create Applications from JAR modal is no longer displayed.
        Assert.assertFalse("Create Applications from JAR modal is still displayed after clicking the Close icon.", createModal.isHeaderDisplayed());

        //Verify the application is not displayed in the table of application JARs.
        Assert.assertFalse("App with name, " + jarApp.getName() + ", displays in JAR Apps table after clicking Close.", jarAppsPage.isAppDisplayed(jarApp));

        //Click the Create button.
        jarAppsPage.clickCreate();

        //Verify default values are displayed in all fields.
        createModal.checkResetValues();

        //Enter a valid value in the JAR Name text box.
        createModal.setJARName(jarApp);

        //Select a valid JAR file using the Browse button. (omitted due to the nature of Selenium interaction - we set the upload file directly rather than using the 'upload'/file browser dialog)
        createModal.setUploadFile(uploadedAppFile);

        //Click the Create button.
        createModal.clickCreateBtnAndWait();

        //Verify the application is displayed in the table of application JARs.
        Assert.assertTrue("App with name, " + jarApp.getName() + " could not be found in JAR Apps table after clicking the Create button.", jarAppsPage.isJARDisplayed(jarApp));

        //Verify the Name column is populated with the value entered previously.
        Assert.assertTrue("New JAR app name is not displayed despite successful creation.", jarAppsPage.isAppNameDisplayed(jarApp.getName()));

        //Verify the appropriate type is displayed in the Device Type column based on the uploaded file.
        DeviceType deviceType = jarApp.getDeviceType();
        Assert.assertEquals("The value in the Type column for the newly created JAR app is not displayed as expected.", deviceType.getUILabel(), jarAppsPage.getDeviceType(fileDisplayName));

        //Verify the newly created application has an auto-generated ID displayed in the ID column.
        Assert.assertNotNull("An ID was not assigned to the new JAR app as expected.", jarAppsPage.getJARAppID(fileDisplayName));

        //Verify the newly created application is selected.
        Assert.assertTrue("Newly created app row not selected as expected.", jarAppsPage.isAppInJARRowSelected(fileDisplayName));

        //Verify the Create, Edit, Delete, and Parameter buttons are all enabled.
        Assert.assertFalse("Create button is not enabled as expected when JAR app is selected.", jarAppsPage.isCreateBtnDisabled());
        Assert.assertFalse("Edit button is not enabled as expected when JAR app is selected.", jarAppsPage.isEditBtnDisabled());
        Assert.assertFalse("Delete button is not enabled as expected when JAR app is selected.", jarAppsPage.isEditBtnDisabled());
        Assert.assertFalse("App Parameters button is not enabled as expected when JAR app is selected.", jarAppsPage.isParametersBtnDisabled());

        //Log out
        jarAppsPage.logout(testUser);
    }

    /**
     * Test steps for ITC-031
     *
     * @throws IOException if errors are encountered reading the file name/path
     *         - likely an issue with file permissions
     */
    @Test
    public void createJarAppTestInternal() throws IOException {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify the Manage Apps page is displayed.
        SimulationsPage simPage = getPage(SimulationsPage.class);
        AppsPage appsPage = simPage.clickApps();
        Assert.assertTrue("Apps header is not displayed as expected.", appsPage.isAppsHeaderDisplayed());

        //Click the User Jar Apps tab.
        JARApplicationsPartialPage jarAppsPage = appsPage.clickJARAppsTab();

        //Click the Create button.
        CreateJARApplicationModal uploadForm = jarAppsPage.clickCreate();

        //With all fields blank, click the Register button.
        uploadForm.clickRegisterBtn();

        //Verify an error is displayed associated with the Jar Name text box and the Upload selector indicating the field is required.
        uploadForm.checkRequiredFieldErrorAllFields();

        //Enter one or more space characters in the Jar Name text box.
        uploadForm.setJARName("  ");
        //Using the Browse button, select a valid jar file.
        uploadForm.setUploadFile(uploadedAppFile);
        //Click the Register button.
        uploadForm.clickRegisterBtn();
        //Verify an error is displayed associated with the Jar Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Required Jar Name error is not displayed as expected when whitespace only is used in Jar Name field.", uploadForm.isNameFieldRequiredErrorDisplayed());

        //Enter a value in the Jar Name text box that contains special characters, not including hyphens, periods, or underscores (e.g., ‘T3$t’).
        uploadForm.setJARName("T3$t!");
        uploadForm.clickRegisterBtn();
        //Verify an error is displayed associated with the Jar Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Jar Name error is not displayed as expected when a name with invalid special characters is used in Jar Name field.", uploadForm.isInvalidAppNameErrorDisplayed());

        //Enter a value in the Jar Name text box that contains consecutive spaces within the entered text (e.g., ‘Te    st’).
        uploadForm.setJARName("Te  st");
        uploadForm.clickRegisterBtn();
        //Verify an error is displayed associated with the Jar Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Jar Name error is not displayed as expected when a name with consecutive spaces is used in Jar Name field.", uploadForm.isInvalidAppNameErrorDisplayed());

        //Enter a value in the Jar Name text box that begins with a space (e.g., ‘ Test’).
        uploadForm.setJARName(" Test");
        uploadForm.clickRegisterBtn();
        //Verify an error is displayed associated with the Jar Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Jar Name error is not displayed as expected when a name with a leading space is used in Jar Name field.", uploadForm.isLeadingTrailingAppNameErrorDisplayed());

        //Enter a value in the Jar Name text box that ends with a space (e.g., ‘Test ’).
        uploadForm.setJARName("Test ");
        uploadForm.clickRegisterBtn();
        //Verify an error is displayed associated with the Jar Name text box indicating the acceptable values for the field.
        Assert.assertTrue("Invalid Jar Name error is not displayed as expected when a name with a trailing space is used in Jar Name field.", uploadForm.isLeadingTrailingAppNameErrorDisplayed());

        //Enter a valid name in the Jar Name text box.
        uploadForm.setJARName(itcJarApp);
        //Using the Browse button, select a non-jar file.
        uploadForm.setUploadFile(zipFile);
        //Verify an error is displayed associated with the Upload selector indicating the field does not support non-jar file types.
        Assert.assertTrue("Invalid File error is not displayed as expected when a non-jar file is attempted to be uploaded.", uploadForm.isInvalidFileErrorDisplayed());

        //Using the Browse button, select an invalid jar file.
        uploadForm.setUploadFile(invalidJarFile);
        //Verify an error is displayed associated with the Upload selector indicating the field does not support the jar file.
        //Assert.assertTrue("Invalid File error is not displayed as expected when an invalid jar file is attempted to be uploaded.", uploadForm.isInvalidJarFileErrorDisplayed()); --Disabled due to BUG 13138

        //Using the Browse button, select a valid jar file.
        uploadForm.setUploadFile(uploadedAppFile);

        //Click the Register button.
        uploadForm.clickCreateBtnAndWait();

        //Verify app is listed in apps table
        Assert.assertTrue("App with name, " + itcJarApp.getName() + " could not be found in User Jar Apps table after clicking register app.", jarAppsPage.isAppDisplayed(itcJarApp.getName()));

        //Click the Add button.
        jarAppsPage.clickCreate();

        //Enter the name of an existing jar app in the Name text box.
        //Using the Browse button, select a valid jar file.
        uploadForm.setJARName(itcJarApp);
        uploadForm.setUploadFile(uploadedAppFile);

        //Click the Register button.
        uploadForm.clickRegisterBtn();

        //Verify an error is displayed associated with the Name text box indicating the field requires a unique value.
        Assert.assertTrue("Duplicate Jar Name error is not displayed as expected when Jar Name text box contains non-unique value.", uploadForm.isDuplicateAppNameErrorDisplayed(itcJarApp.getName()));
        //Click the Cancel button.
        uploadForm.clickCancelBtn();

        //Logout
        jarAppsPage.logout(testUser);
    }
}
