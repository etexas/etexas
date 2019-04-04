package com.harmonia.qa.ETEXASWebQATests.AppManagementTests;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.ApplicationParametersModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.EmbeddedAppsPartialPage;

/**
 * Test class which executes steps for the Viewing Built In Apps test, TC-045
 *
 * @author llaroussini
 * @author saistrop
 */
public class ViewEmbeddedApplicationsTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * List of built in apps
     */
    private List<EmbeddedApp> apps;

    /**
     * Integer used to store a random value for selecting an app
     */
    private int randomNum;

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
        apps = ETexasEntityManager.getAllEmbeddedApps();

        //User is logged in and on Apps Page
        SimulationsPage simPage = landing.loginAs(testUser);
        simPage.clickApps();
    }

    /**
     * Test steps for TC-045
     */
    @Test
    public void viewEmbeddedAppsExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Ensure the Apps page is loaded
        AppsPage appsPage = getPage(AppsPage.class);
        Assert.assertTrue("Connected Vehicle Applications header is not displayed as expected.", appsPage.isAppsHeaderDisplayed());

        //Verify all expected tabs display
        appsPage.checkAllTabs();

        //Click the Embedded Apps tab
        EmbeddedAppsPartialPage embeddedAppsPage = appsPage.clickEmbeddedAppsTab();

        //Verify Parameter button is displayed and disabled by default
        Assert.assertTrue("Parameters button is not displayed after loading the Embedded Applications page when expected.", embeddedAppsPage.isParametersBtnDisplayed());
        Assert.assertTrue("Parameter button is not disabled after loading Embedded Applications page when expected.", embeddedAppsPage.isParametersBtnDisabled());

        //Verify table displays for embedded apps
        embeddedAppsPage.checkAllEmbeddedAppsDisplayed(apps);

        //Verify column headers are displayed as part of test step 8
        embeddedAppsPage.checkAllColumnHeaders("8");

        //Verify each app has a unique ID listed in the ID column
        embeddedAppsPage.checkAllEmbeddedAppIDsUnique(apps);

        //Verify all existing embedded apps display a name and Device Type
        embeddedAppsPage.checkAllEmbeddedAppsDisplayed(apps);
        embeddedAppsPage.checkAllEmbeddedAppTypesDisplayed(apps);

        //Gets a random app to be selected in the test execution
        randomNum = ThreadLocalRandom.current().nextInt(1, (apps.size()));

        //Click any app and verify Parameters button becomes enabled
        EmbeddedApp app = apps.get(randomNum);
        String appName = app.getName();
        embeddedAppsPage.selectApp(appName);
        Assert.assertFalse("Parameters button is not enabled when expected.", embeddedAppsPage.isParametersBtnDisabled());

        //Clicks the Parameters button and verifies the Application Parameters modal appears
        ApplicationParametersModal appParamWindow = embeddedAppsPage.clickParametersBtn(appName);
        Assert.assertTrue("Application Parameters modal for built in app named, " + appName + ", not displayed as expected after clicking parameters button.",
                appParamWindow.isAppParametersHeaderDisplayed());

        //Verify help and close icons display in header
        Assert.assertTrue("Application Parameters modal does not display the Help icon where expected.", appParamWindow.isAppParametersHelpIconDisplayed());
        Assert.assertTrue("Application Parameters modal does not display the Close icon where expected.", appParamWindow.isCloseBtnDisplayed());

        //Click the help icon, verify help window displays
        appParamWindow.clickAppParametersHelpIcon();
        Assert.assertTrue("View App Parameters Help header not displayed after clicking the help icon.", appParamWindow.isAppParametersHelpHeaderDisplayed());
        Assert.assertTrue("View App Parameters Help content not displayed after clicking the help icon.", appParamWindow.isAppParametersHelpContentDisplayed());

        //Verify Help OK button displays, click OK, verify help window closes
        Assert.assertTrue("OK button could not be found in View App Parameters help window.", appParamWindow.isHelpOKBtnDisplayed());
        appParamWindow.clickHelpOKBtn();
        Assert.assertFalse("View App Parameters Help header is still displayed after clicking the OK button.", appParamWindow.isAppParametersHelpHeaderDisplayed());

        //Verify parameters for the application display, or that text appears stating there are no parameters set.
        appParamWindow.checkAppParameters(app);

        //Verify app parameter columns display for Parameter Name and Default Value
        appParamWindow.checkColumnHeaderCells();

        //Verify a Close button is displayed
        Assert.assertTrue("Close button not displayed in Viewing App Parameters window.", appParamWindow.isCloseBtnDisplayed());

        //Verify expected parameters display for specified app in modal
        appParamWindow.checkAppParameters(app);

        //Click the close button
        appParamWindow.clickCloseButton();

        //Verify Application Parameters modal is no longer displayed
        Assert.assertFalse("Application Parameter modal is still displayed after clicking the Close button.", appParamWindow.isAppParametersHeaderDisplayed());

        //Verify application is still selected, and if not select it
        if (!embeddedAppsPage.isAppSelected(appName)) {
            embeddedAppsPage.selectApp(appName);
        }

        //Click the parameters button and select the 'x' icon
        appParamWindow = embeddedAppsPage.clickParametersBtn(appName);
        appParamWindow.clickCloseIcon();

        //Verify Application Parameters modal is no longer displayed after clicking 'x' icon
        Assert.assertFalse("Application Parameter modal is still displayed after clicking the 'x' icon.", appParamWindow.isAppParametersHeaderDisplayed());

        //Log out
        embeddedAppsPage.logout(testUser);
    }
}
