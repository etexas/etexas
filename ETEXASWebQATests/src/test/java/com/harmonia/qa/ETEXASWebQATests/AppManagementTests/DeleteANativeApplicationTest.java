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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal.Btn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.NativeAppsPartialPage;

/**
 * Executes steps of Delete A Native Application Test, TC-088
 *
 * @author llaroussini
 */
public class DeleteANativeApplicationTest extends ETexasAfterTestResetTestBase {

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
     * Test steps for TC-088: Delete a Native Application
     */
    @Test
    public void deleteNativeApplicationExternalTest() {
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

        //Verify the Delete button is enabled.
        Assert.assertFalse("Delete button is not enabled as expected when native app is selected", nativeTab.isDeleteBtnDisabled());

        //Click the Delete button.
        ConfirmDeleteModal deleteModal = nativeTab.clickDelete();

        //Verify a Confirm Delete modal is displayed with text confirming deletion of the selected application.
        deleteModal.checkDeleteWarningNativeAppContent(app);

        //Verify an ‘x’ icon is displayed in the top right corner of the modal.
        Assert.assertTrue("The 'x' icon is not displayed in Confirm Delete modal as expected.", deleteModal.isCloseIconDisplayed());

        //Verify that Yes and No buttons are displayed at the bottom of the modal.
        deleteModal.checkConfirmDeleteBtns();

        //Click the No button.
        deleteModal.clickBtn(Btn.NO);

        //Verify the Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking the No button.", deleteModal.isNativeAppDeletionContentDisplayed(app));

        //Verify the application is still displayed in the table of native applications.
        Assert.assertTrue("Native Application named: " + appName + " is no longer displayed despite cancelling deletion.", nativeTab.isAppDisplayed(appName));

        //Verify the application is still selected, if not, select the application.
        if (nativeTab.isNativeAppRowSelected(app) == false) {
            nativeTab.selectNativeApp(appName, true);
        }

        //Click the Delete button again.
        nativeTab.clickDelete();

        //Verify the Confirm Delete modal is displayed.
        deleteModal.checkDeleteWarningNativeAppContent(app);

        //Click the ‘x’ icon.
        deleteModal.clickCloseIcon();

        //Verify the Confirm Delete modal is no longer displayed.
        Assert.assertFalse("Delete Warning window is still displayed after clicking the 'x' icon.", deleteModal.isNativeAppDeletionContentDisplayed(app));

        //Verify the application is still displayed in the table of native applications.
        Assert.assertTrue("Native Application named: " + appName + " is no longer displayed despite closing the Confirm Delete modal before confirming deletion.", nativeTab.isAppDisplayed(appName));

        //Verify the application is still selected, if not, select the application.
        if (nativeTab.isNativeAppRowSelected(app) == false) {
            nativeTab.selectNativeApp(appName, true);
        }

        //Click the Delete button again.
        nativeTab.clickDelete();

        //Verify the Confirm Delete modal is displayed.
        deleteModal.checkDeleteWarningNativeAppContent(app);

        //Click the Yes button.
        deleteModal.clickBtn(Btn.YES);

        //Verify the application no longer is displayed in the table of native applications.
        Assert.assertFalse("Native Application named: " + appName + " is still displayed despite confirming deletion.", nativeTab.isAppDisplayed(appName));

        //Logout
        nativeTab.logout(testUser);
    }
}
