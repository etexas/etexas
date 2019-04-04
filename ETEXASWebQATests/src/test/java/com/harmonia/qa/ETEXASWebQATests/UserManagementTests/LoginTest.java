package com.harmonia.qa.ETEXASWebQATests.UserManagementTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the User Login test, TC-002
 *
 * @author llaroussini
 * @author rsmith
 */
public class LoginTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");
        testUser = ETexasUserFactory.getUser(true); //Get a random user.

        ETexasUserUtils.userRegistration(testUser);

    }

    /**
     * Test steps for TC-002
     */
    //@Test
    //TODO - Update this test once Simulation and Composite buttons have been separated out and related methods updated
    public void loginExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");
        //Assert landing page loads with login form
        LandingPage landingPage = getPage(LandingPage.class);
        Assert.assertTrue("Login form is missing.", landingPage.isLoginFormDisplayed());

        //Login and assert simulations page section displays (including main header, links, and correct username)
        landingPage.setUsernameAndPassword(testUser);
        SimulationsPage simulations = landingPage.clickLogin();
        simulations.checkDashboard(testUser);

        //Verify Simulations page displays with Simulations and Executions sections
        simulations.checkSimulationsHeaderText();
        //simulations.checkExecutionsHeaderText(); //TODO - not yet implemented in eTEXAS version 3.0

        //Verify Simulation section displays appropriate action buttons, with only the New button enabled
        simulations.checkAllSimulationBtns();
        simulations.checkDisabledSimulationBtns();

        //Verify Execution section displays appropriate action buttons, with all disabled
        //simulations.checkAllExecutionBtns(); //TODO - not yet implemented in eTEXAS version 3.0
        //simulations.checkDisabledCompletedExecutionBtns(); //TODO - not yet implemented in eTEXAS version 3.0

        //Click the username, verify logout options displays
        simulations.clickUsername(testUser);
        Assert.assertTrue("Logout link is not displayed,", simulations.isLogoutDisplayed());

        //Click log out and verify login page displays
        simulations.clickLogout();

    }

    /**
     * Test steps for ITC-002
     */
    @Test
    public void loginInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Assert landing page loads with login form
        LandingPage landingPage = getPage(LandingPage.class);
        Assert.assertTrue("Login form is missing.", landingPage.isLoginFormDisplayed());

        //With login fields blank, click Login
        landingPage.clickLoginNoRtn();

        //Verify 'field required' errors display associated with username and password fields
        landingPage.checkUsernameFieldRequiredErrorDisplayed();
        landingPage.checkPasswordFieldRequiredErrorDisplayed();

        //Enter whitespace only in username and password fields, click Login
        landingPage.setUsernameText("    ");
        landingPage.setPasswordText("    ");
        landingPage.clickLoginNoRtn();

        //Verify 'field required' errors display associated with username and password fields
        landingPage.checkUsernameFieldRequiredErrorDisplayed();
        landingPage.checkPasswordFieldRequiredErrorDisplayed();

        //Enter an invalid username and any password value, click Login.
        landingPage.setUsernameText(RandomStringGenerator.nextLetterString(10));
        landingPage.setPasswordText(RandomStringGenerator.nextLetterString(10));
        landingPage.clickLoginNoRtn();

        //Verify an error is displayed indicating login failed and to try again
        landingPage.isLoginFailedErrorDisplayed();
        //TODO - remove once updates are made to REST API, this failure message is only displayed due to discrepancies between front-end and REST API validation.
        if (landingPage.isFailureMessageDisplayed()) {
            landingPage.clickFailureOK();
        }

        //Enter a valid username and an invalid password value, click Login
        landingPage.setUsernameText(testUser.getUsername());
        landingPage.setPasswordText(RandomStringGenerator.nextLetterString(10));
        landingPage.clickLoginNoRtn();

        //Verify an error is displayed indicating login failed and to try again
        landingPage.isLoginFailedErrorDisplayed();
        //TODO - remove once updates are made to REST API, this failure message is only displayed due to discrepancies between front-end and REST API validation.
        if (landingPage.isFailureMessageDisplayed()) {
            landingPage.clickFailureOK();
        }

        //Enter valid values in Login fields, click Login
        landingPage.setUsernameAndPassword(testUser);
        SimulationsPage simulations = landingPage.clickLogin();

        //Verify Simulations page displays
        simulations.checkSimulationsHeaderText();

        //Logout
        simulations.logout(testUser);

    }
}
