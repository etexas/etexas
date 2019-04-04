package com.harmonia.qa.ETEXASWebQATests.UserManagementTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.NewUserForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.RegisterSuccessWindow;
import com.harmonia.qa.Utilities.RandomDataGenerators.RandomStringGenerator;

/**
 * Test class which executes steps for the User Registration test, TC-001
 *
 * @author cbulloss
 * @author llaroussini
 * @author rsmith
 * @author saistrop
 */
public class UserRegistrationTest extends ETexasAfterTestResetTestBase {

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
    }

    /**
     * Test steps for TC-001
     */
    @Test
    public void userRegistrationExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Verify landing page is displayed with login form
        LandingPage landingPage = ETexasCommonUtils.goToLandingPage();
        Assert.assertTrue("Login form is missing.", landingPage.isLoginFormDisplayed());

        //Click Need an Account? Sign Up.
        NewUserForm userForm = landingPage.clickSignUp();

        //Verify New User form is displayed
        Assert.assertTrue("Header is not displayed on the new user form.", userForm.isHeaderDisplayed());

        //Verify that an 'x' icon is displayed in the upper right corner
        Assert.assertTrue("x icon is not displayed in the upper right corner of modal.", userForm.isCloseIconDisplayed());

        //Verify all expected fields display
        userForm.checkForm();

        //Verify fields are marked as 'required' - TODO disabled due to forms no longer marked as required, checking with Dev to see if this was an intended change.
        //userForm.checkRequiredFields();

        //Verify Register and Cancel buttons are displayed
        Assert.assertTrue("Register button is not displayed on the user form.", userForm.isRegisterBtnDisplayed());
        Assert.assertTrue("Cancel button is not displayed on the user form.", userForm.isCancelBtnDisplayed());

        //Enter values, click Cancel and verify form is closed
        userForm.populateForm(testUser);
        userForm.clickCancelBtn();
        Assert.assertFalse("User form is still displayed after clicking cancel.", userForm.isHeaderDisplayed());

        //Click Sign Up Link
        userForm = landingPage.clickSignUp();

        //Verify the Create a User Account for eTEXAS modal pops up again.
        Assert.assertTrue("Header is not displayed on the new user form.", userForm.isHeaderDisplayed());

        //Verify all fields are cleared.
        userForm.checkClearedFields();

        //Enter values, click close and verify form is closed
        userForm.populateForm(testUser);
        userForm.clickCloseIcon();
        Assert.assertFalse("User form is still displayed after clicking cancel.", userForm.isHeaderDisplayed());

        //Click Sign Up link, verify fields are cleared, and enter valid values and click Register
        userForm = landingPage.clickSignUp();
        userForm.checkClearedFields();
        userForm.populateForm(testUser);
        userForm.checkDisplayedSetFields(testUser);
        RegisterSuccessWindow successWindow = userForm.clickRegisterBtnRtrnSuccess();

        //Verify success window is displayed and click OK
        Assert.assertTrue("Success window not displayed after clicking the register button.", successWindow.isHeaderDisplayed());
        successWindow.clickOk();

        //Verify success window closes
        Assert.assertFalse("Success window did not disappear after clicking ok.", successWindow.isHeaderDisplayed());

        //All below steps are covered by verifyUser method.
        //Navigate to the email account used during registration.
        //Verify an email was received from eTEXAS providing a link for email verification.
        //Click the link in the email to verify the email address.
        //Verify a message is displayed indicating that the email address has been verified.
        ETexasUserUtils.verifyUser(testUser);

    }

    /**
     * Test steps for ITC-001
     */
    // @Test - Disabled due to BUG 13728
    public void userRegistrationInternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Internal Test Case");

        //Verify landing page is displayed with login form
        LandingPage landingPage = ETexasCommonUtils.goToLandingPage();
        Assert.assertTrue("Login form is missing.", landingPage.isLoginFormDisplayed());

        //Click Need an Account? Sign Up
        NewUserForm userForm = landingPage.clickSignUp();

        //Register a new user -- not in test steps, but used for testing of duplicate username
        userForm.populateForm(testUser);
        RegisterSuccessWindow successWindow = userForm.clickRegisterBtnRtrnSuccess();
        successWindow.clickOk();

        //Click New User again
        landingPage.clickSignUp();

        //With all fields blank, click Register
        userForm.clickRegisterBtn();

        //Verify errors display associated with all fields (with the exception of Organization)
        userForm.checkRequiredFieldErrorAllFields();

        //Enter one or more space characters in each text box., click Register
        userForm.setUsernameText("   ");
        userForm.setPasswordAndConfirm("   ");
        userForm.setEmailText("   ");
        userForm.setFirstNameText("   ");
        userForm.setLastNameText("   ");
        userForm.clickRegisterBtn();

        //Verify errors display associated with all fields (with the exception of Organization)
        userForm.checkRequiredFieldErrorAllFields();

        //Enter a username already in use by another user and valid values in remaining fields then click Register
        userForm.populateForm(testUser);
        userForm.setEmailText(RandomStringGenerator.getRandomEmailAddress());
        userForm.clickRegisterBtn();

        //Verify error displays for duplicate username, click OK and verify error window closes
        Assert.assertTrue("Registration error window not displayed as expected after entering a duplicate username.", userForm.isRegistrationFailedWindowDisplayed());
        userForm.checkUsernameInErrorMessageText(testUser.getUsername());
        userForm.clickRegistrationErrorOKBtn();
        Assert.assertFalse("Registration error window still displayed after clicking OK.", userForm.isRegistrationFailedWindowDisplayed());

        //Enter a username containing special special characters in the Username text box.
        userForm.setUsernameText(RandomStringGenerator.nextString(10, true));
        userForm.clickRegisterBtn();

        //Verify an error is displayed associated with Username text box indicating the acceptable values for the field.
        userForm.checkUsernameFieldError();

        //Enter a value in the Username text box that begins with a digit.
        userForm.setUsernameText(RandomStringGenerator.nextNumStringOfLength(10) + RandomStringGenerator.nextString());

        //Enter a valid username, enter a confirm password that does NOT match the password field then click Register
        userForm.setUsernameText(RandomStringGenerator.nextLetterString(10));
        userForm.setConfirmPasswordText(RandomStringGenerator.nextString(5, true));
        userForm.clickRegisterBtn();

        //Verify error on confirm password field
        Assert.assertTrue("Password match error tooltip could not be found after entering different password and confirm password values.", userForm.isPasswordMatchErrorDisplayed());

        //Enter a valid password in the confirm password field
        userForm.setConfirmPasswordText(testUser.getPassword());

        //Enter an incomplete email address in the Email field and click Register
        userForm.setEmailText("test@harmonia");
        userForm.clickRegisterBtn();

        //Verify error on email field
        Assert.assertTrue("Invalid email error tooltip could not be found after entering an incomplete email address.", userForm.isInvalidEmailErrorDisplayed());

        //Enter a valid email and click Register
        userForm.setEmailText(testUser.getEmailAddress());
        userForm.clickRegisterBtnRtrnSuccess();

        //Verify success window is displayed and click OK
        Assert.assertTrue("Success window not displayed after clicking the register button.", successWindow.isHeaderDisplayed());
        successWindow.clickOk();

        //Verify success window closes
        Assert.assertFalse("Success window did not disappear after clicking ok.", successWindow.isHeaderDisplayed());

    }
}
