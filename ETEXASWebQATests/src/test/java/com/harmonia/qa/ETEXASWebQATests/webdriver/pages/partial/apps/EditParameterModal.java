package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps;

import junit.framework.Assert;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.BtnNames;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.BaseForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DeviceApplicationsModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the Edit Parmeter Modal
 *
 * @author llaroussini
 */
public class EditParameterModal extends BaseForm {

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public EditParameterModal(WebDriver driver) {
        super(driver);
    }

    //////////////
    // IDs and Locators
    //////////////

    /**
     * Text displayed in the Edit Parameter modal header
     */
    private static final String EDIT_PARAMETER_HEADER_TEXT = "Edit Parameter";

    /**
     * Text displayed in the Edit Parameter Help modal header
     */
    private static final String EDIT_PARAMETER_HELP_HEADER_TEXT = "Edit Parameter Help";

    /**
     * Text displayed in the Edit Parameter Help modal content
     */
    private static final String EDIT_PARAMETER_HELP_CONTENT_TEXT = "Edit the value of the selected parameter.";

    /**
     * Xpath to the displayed name value
     */
    private static final String DISPLAYED_PARAMETER_NAME_AREA_XPATH = "//div[contains(@id, 'parameter')][contains(@id, 'Edit')][contains(@id, 'name')][@data-ref='inputEl']";

    /**
     * Xpath to the Value text box
     */
    private static final String VALUE_TEXT_BOX_XPATH = "//input[contains(@id, 'parameter')][contains(@id, 'Edit')][contains(@id, 'value')]";

    /**
     * By locator suitable for locating the Loading alert. Note that this is not
     * guaranteed to be unique, and may return multiple elements which are not
     * currently displayed.
     */
    private static final By LOADING_ALERT_LOCATOR = By.xpath(".//div[contains(@id,'loadmask')][text()='Loading...']");

    /**
     * Parameter Value field name as displayed in UI
     */
    private static final String PARAMETER_VALUE_FIELD_DISPLAYED_NAME = "Value";

    //////////////
    // Getters
    //////////////

    /**
     * Gets the Parameter Name area
     *
     * @return the are displaying the parameter name
     */
    private El getParamNameArea() {
        return el(By.xpath(DISPLAYED_PARAMETER_NAME_AREA_XPATH));
    }

    /**
     * Gets the Parameter Value text box
     *
     * @return the parameter value text box
     */
    private El getParamValueTextBox() {
        return el(By.xpath(VALUE_TEXT_BOX_XPATH));
    }

    /**
     * Gets the Update button
     *
     * @return the update button
     */
    private El getUpdateBtn() {
        return getFormBtn(EDIT_PARAMETER_HEADER_TEXT, BtnNames.UPDATE.getLabel());
    }

    /**
     * Gets the Reset button
     *
     * @return the reset button
     */
    private El getResetBtn() {
        return getFormBtn(EDIT_PARAMETER_HEADER_TEXT, BtnNames.RESET.getLabel());
    }

    /**
     * Gets the Cancel button
     *
     * @return the Cancel button
     */
    private El getCancelBtn() {
        return getFormBtn(EDIT_PARAMETER_HEADER_TEXT, BtnNames.CANCEL.getLabel());
    }

    /**
     * Gets the displayed parameter name
     *
     * @return the displayed parameter name
     */
    public String getParameterName() {
        return getParamNameArea().getText();
    }

    /**
     * Gets the Parameter Value as it was initially loaded (not necessarily as
     * it has been set since opening)
     *
     * @return the parameter value
     */
    public String getParameterValue() {
        return getParamValueTextBox().getAttribute("value");
    }


    //////////////
    // Checkers
    //////////////

    /**
     * Checks the header icons for this form
     */
    public void checkHeaderIcons() {
        checkHeaderIcons(EDIT_PARAMETER_HEADER_TEXT);
    }

    /**
     * Checks to see if the parameter name area is displayed
     *
     * @return true if the parameter name area is displayed, false otherwise
     */
    public boolean isParameterNameAreaDisplayed() {
        return isElementDisplayed(getParamNameArea());
    }

    /**
     * Checks to see if the Parameter Value text box is displayed
     *
     * @return true if the parameter value text box is displayed, false
     *         otherwise
     */
    public boolean isParameterValueTextBoxDisplayed() {
        return isElementDisplayed(getParamValueTextBox());
    }

    /**
     * Checks to see if the Update button is displayed
     *
     * @return true if the Update button is displayed, false otherwise
     */
    public boolean isUpdateBtnDisplayed() {
        return isElementDisplayed(getUpdateBtn());
    }

    /**
     * Checks to see if the Reset button is displayed
     *
     * @return true if the Reset button is displayed, false otherwise
     */
    public boolean isResetBtnDisplayed() {
        return isElementDisplayed(getResetBtn());
    }

    /**
     * Checks to see if the cancel button is displayed
     *
     * @return true if the cancel button is displayed, false otherwise
     */
    public boolean isCancelBtnDisplayed() {
        return isElementDisplayed(getCancelBtn());
    }

    /**
     * Checks to see if the header is displayed with the expected text
     *
     * @return true if the header is displayed, false otherwise
     */
    public boolean isHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_PARAMETER_HEADER_TEXT);
    }

    /**
     * Checks to see if the help header is displayed with the expected text
     *
     * @return true if the help header is displayed, false otherwise
     */
    public boolean isHelpHeaderDisplayed() {
        return isHeaderDisplayed(EDIT_PARAMETER_HELP_HEADER_TEXT);
    }

    /**
     * Checks to see if the help content is displayed with the expected text
     *
     * @return true if the help content is displayed, false otherwise
     */
    public boolean isHelpContentDisplayed() {
        return isContentDisplayed(EDIT_PARAMETER_HELP_CONTENT_TEXT);
    }

    /**
     * Checks to see if the ok button is displayed in help modal
     *
     * @return true if the button is displayed, false otherwise
     */
    public boolean isHelpOKBtnDisplayed() {
        return isHelpOKBtnDisplayed(EDIT_PARAMETER_HELP_HEADER_TEXT);
    }

    /**
     * Checks if field required error is displayed with Value text box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isValueRequiredErrorDisplayed() {
        return isFieldRequiredErrorDisplayed(PARAMETER_VALUE_FIELD_DISPLAYED_NAME);
    }

    /**
     * Checks if leading/trailing whitespace error is displayed with Value text
     * box
     *
     * @return true if displayed, false otherwise
     */
    public boolean isValueLeadingTrailingWhitespaceErrorDisplayed() {
        return isWhitespaceErrorDisplayed(PARAMETER_VALUE_FIELD_DISPLAYED_NAME);
    }

    //////////////
    // Interaction
    //////////////

    /**
     * Clicks the help icon in the Edit Parameter modal
     */
    public void clickHelp() {
        clickHelpIcon(EDIT_PARAMETER_HEADER_TEXT);
    }

    /**
     * Clicks the close icon
     */
    public void clickCloseIcon() {
        clickCloseIcon(EDIT_PARAMETER_HEADER_TEXT);
    }

    /**
     * Sets the text in the parameter value text box
     *
     * @param value the value to set
     */
    public void setParameterValue(String value) {
        getParamValueTextBox().setText(value);
    }

    /**
     * Clicks the update button - does not wait for loading icon or window to
     * close
     *
     * @param success true if success expected, otherwise false
     * @return Device Application Modal if success, otherwise return is null
     */
    public DeviceApplicationsModal clickUpdate(boolean success) {
        getUpdateBtn().click();
        if (success) {
            waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + EDIT_PARAMETER_HEADER_TEXT + "']"));
            return getPage(DeviceApplicationsModal.class);
        }
        else {
            return null;
        }
    }

    /**
     * Clicks the reset button
     */
    public void clickReset() {
        getResetBtn().click();
    }

    /**
     * Clicks the cancel button
     */
    public void clickCancel() {
        getCancelBtn().click();
    }

    /**
     * Clicks the OK button in the help window
     */
    public void clickHelpOKBtn() {
        clickHelpOKBtn(EDIT_PARAMETER_HELP_HEADER_TEXT);
    }

    //////////////
    // Utilities
    //////////////

    /**
     * Checks to see if the parameter name and value fields are displayed in the
     * modal
     */
    public void checkFields() {
        Assert.assertTrue("The Parameter Name area is not displayed in the Edit Parameter modal.", isParameterNameAreaDisplayed());
        Assert.assertTrue("The Parameter Value text box is not displayed in the Edit Parameter modal.", isParameterValueTextBoxDisplayed());
    }

    /**
     * Checks that help modal is displayed -- checks both the header and content
     * text
     */
    public void checkHelpModal() {
        Assert.assertTrue("Edit Parameter Help header not displayed as expected.", isHelpHeaderDisplayed());
        Assert.assertTrue("Edit Parameter Help content not displayed as expected.", isHelpContentDisplayed());
    }

    /**
     * Checks to see if the Update, Reset, and Cancel buttons are displayed on
     * the form
     */
    public void checkBtns() {
        Assert.assertTrue("The Update button is not displayed on the form.", isUpdateBtnDisplayed());
        Assert.assertTrue("The Reset button is not displayed on the form.", isResetBtnDisplayed());
        Assert.assertTrue("The Cancel button is not displayed on the form.", isCancelBtnDisplayed());
    }

    /**
     * Verifies the given name is displayed in the Parameter Name area in the
     * Edit Parameter modal
     *
     * @param name -the name expected
     */
    public void checkParameterNameDisplayed(String name) {
        Assert.assertEquals("The parameter name: " + name + " was not found in the Parameter Name area in the Edit Parameter modal.", name, getParameterName());
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Parameter Value
     * text box
     */
    public void checkParamValueFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Parameter Value text box.", isFieldRequiredErrorDisplayed(PARAMETER_VALUE_FIELD_DISPLAYED_NAME));
    }

    /**
     * Waits for the "Loading..." alert to be displayed
     */
    public void waitForAlert() {
        waitForElementToBeVisible(LOADING_ALERT_LOCATOR);
    }

}
