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
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.ApplicationParametersModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.NativeAppsPartialPage;

/**
 * Executes steps of View Native Application Parameters test, TC-087
 *
 * @author llaroussini
 */
public class ViewNativeApplicationParametersTest extends ETexasAfterTestResetTestBase {

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
     * Test steps for TC-087: View Native Application Parameters
     */
    @Test
    public void nativeApplicationParametersExternalTest() {
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

        //Verify the Parameters button is enabled.
        Assert.assertFalse("Parameters button is not enabled as expected when native app is selected", nativeTab.isParametersBtnDisabled());

        //Click the Parameters button.
        ApplicationParametersModal paramModal = nativeTab.clickParameters();

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

        //Verify a table of app parameters, if any exist, is displayed in the Application Parameters modal; if no parameters exist, verify text is displayed indicating no parameters are defined for the application.
        if (app.getParameters().size() == 0) {
            Assert.assertFalse("The Parameters table is displayed in the Application Parameters modal for native app named: " + appName + ", which is NOT expected to have parameters.",
                    paramModal.isNoParametersAreaDisplayed());
        }
        else {
            Assert.assertTrue("No parameters are displayed in the Application Parameters modal for native app named: " + appName + ", which is expected to have parameters",
                    paramModal.isAppParameterTableDisplayed());
        }

        //Verify the following columns are displayed in the Application Parameters modal: Name and Default Value.
        paramModal.checkColumnHeaderCells();

        //Verify a Close button is displayed at the bottom of the modal.
        Assert.assertTrue("Close button is not displayed as expected in Application Parameters modal.", paramModal.isCloseBtnDisplayed());

        //If any parameters exist for the selected app, verify they are listed with the correct parameter name and default value in the table.
        paramModal.checkAppParameters(app);

        //Click the Close button.
        paramModal.clickCloseButton();

        //Verify the Application Parameters modal is no longer displayed.
        Assert.assertFalse("The Application Parameters modal is still displayed ater clicking the Close button.", paramModal.isAppParametersHeaderDisplayed());

        //Verify the application is still selected, if not, select the application.
        if (nativeTab.isNativeAppRowSelected(app) == false) {
            nativeTab.selectNativeApp(appName, true);
        }

        //Click the Parameters button.
        nativeTab.clickParameters();

        //Click the ‘x’ icon.
        paramModal.clickCloseIcon();

        //Verify the Application Parameters modal is no longer displayed.
        Assert.assertFalse("The Application Parameters modal is still displayed ater clicking the 'x' icon.", paramModal.isAppParametersHeaderDisplayed());

        //Logout
        nativeTab.logout(testUser);
    }
}