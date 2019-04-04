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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.JARApplicationsPartialPage;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Executes steps of Delete a JAR of Applications Test, TC-094
 *
 * @author llaroussini
 */
public class DeleteAJARofApplicationsTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Jar app used in test case - with a single app
     */
    private UserJarApp jarApp;

    /**
     * Name of the JAR
     */
    private String jarName;

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
        jarName = jarApp.getName();
        ETexasEntityManager.addEntities(testUser, jarApp);

        //Register user
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);

        //Create JAR app in UI
        ETexasAppUtils.createJARApp(jarApp, uploadedAppFile);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-094: Delete a JAR of Applications
     */
    @Test
    public void deleteJARExternalTest() {
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

        //Select any listed JAR with a single application.
        jarTab.selectRow(fileDisplayName, true);

        //Verify the Delete button is enabled.
        Assert.assertFalse("Delete button is not enabled as expected when JAR application is selected.", jarTab.isDeleteBtnDisabled());

        //Click the Delete button.
        ConfirmDeleteModal deleteModal = jarTab.clickDelete();

        //Verify a Confirm Delete modal is displayed with text confirming deletion of the selected JAR of applications.
        deleteModal.checkDeleteWarningJarAppContent(jarName, fileDisplayName);

        //Verify an ‘x’ icon is displayed in the top right corner of the modal.
        Assert.assertTrue("The 'x' icon is not displayed in Confirm Delete modal as expected.", deleteModal.isCloseIconDisplayed());

        //Verify that Yes and No buttons are displayed at the bottom of the modal.
        deleteModal.checkConfirmDeleteBtns();

        //Click the No button.
        deleteModal.clickBtn(Btn.NO);

        //Verify the Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking the No button.", deleteModal.isJarAppDeletionContentDisplayed(jarName, fileDisplayName));

        //Verify the JAR of application is still displayed in the table of application JARs.
        Assert.assertTrue("JAR Application named: " + fileDisplayName + " is no longer displayed despite cancelling deletion.", jarTab.isAppDisplayed(fileDisplayName));

        //Verify the application is still selected, if not, select the application.
        if (jarTab.isAppInJARRowSelected(fileDisplayName) == false) {
            jarTab.selectAppInJAR(fileDisplayName, true);
        }

        //Click the Delete button again.
        jarTab.clickDelete();

        //Verify the Confirm Delete modal is displayed.
        deleteModal.checkDeleteWarningJarAppContent(jarName, fileDisplayName);

        //Click the ‘x’ icon.
        deleteModal.clickCloseIcon();

        //Verify the Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking the 'x' icon.", deleteModal.isJarAppDeletionContentDisplayed(jarName, fileDisplayName));

        //Verify the JAR of application is still displayed in the table of application JARs.
        Assert.assertTrue("JAR Application named: " + fileDisplayName + " is no longer displayed despite closing the Delete modal.", jarTab.isAppDisplayed(fileDisplayName));

        //Verify the application is still selected, if not, select the application.
        if (jarTab.isAppInJARRowSelected(fileDisplayName) == false) {
            jarTab.selectAppInJAR(fileDisplayName, true);
        }

        //Click the Delete button again.
        jarTab.clickDelete();

        //Verify the Confirm Delete modal is displayed.
        deleteModal.checkDeleteWarningJarAppContent(jarName, fileDisplayName);

        //Click the Yes button.
        deleteModal.clickBtn(Btn.YES);

        //Verify the JAR and associated application are no longer displayed in the table of application JARs.
        Assert.assertFalse("JAR Application named: " + fileDisplayName + " is still displayed despite confirming deletion.", jarTab.isAppDisplayed(fileDisplayName));

        //TODO- this type of JAR does not currently exist for version 3.0, update once available.
        //Select any listed JAR with multiple applications.
        //Verify the Delete button is enabled.
        //Click the Delete button.
        //Verify a Confirm Delete modal is displayed with text confirming deletion of the selected JAR of applications.
        //Verify an ‘x’ icon is displayed in the top right corner of the modal.
        //Verify that Yes and No buttons are displayed at the bottom of the modal.
        //Click the No button.
        //Verify the Confirm Delete modal is no longer displayed.
        //Verify the JAR of application is still displayed in the table of application JARs.
        //Verify the application is still selected, if not, select the application.
        //Click the Delete button again.
        //Verify the Confirm Delete modal is displayed.
        //Click the ‘x’ icon.
        //Verify the Confirm Delete modal is no longer displayed.
        //Verify the JAR of application is still displayed in the table of application JARs.
        //Verify the application is still selected, if not, select the application.
        //Click the Delete button again.
        //Verify the Confirm Delete modal is displayed.
        //Click the Yes button.
        //Verify the JAR and ALL associated applications are no longer displayed in the table of application JARs.

        //Log Out.
        jarTab.logout(testUser);
    }
}
