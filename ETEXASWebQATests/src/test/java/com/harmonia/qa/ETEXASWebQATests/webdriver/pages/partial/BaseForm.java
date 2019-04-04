package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial;

import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.ETexasBasePage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class to hold elements common to all Form pages
 *
 * @author llaroussini
 * @author cbulloss
 */
public abstract class BaseForm extends ETexasBasePage {

    /**
     * The header text displayed on at the top of the form
     */
    public static String HEADER_TEXT;

    /**
     * The help text expected to be displayed in the help window for this form.
     */
    public static String HELP_TEXT;

    /**
     * Default constructor
     *
     * @param driver the web driver being used
     */
    public BaseForm(WebDriver driver) {
        super(driver);
    }

    //////////////
    //Enumerations
    //////////////

    /**
     * Enumeration of form buttons
     *
     * @author llaroussini
     */
    public enum FormBtn {
        /**
         * Create button
         */
        CREATE("Create"),
        /**
         * Edit button
         */
        EDIT("Edit"),
        /**
         * Delete button
         */
        DELETE("Delete"),
        /**
         * Reset button
         */
        RESET("Reset"),
        /**
         * Cancel button
         */
        CANCEL("Cancel"),
        /**
         * Update button
         */
        UPDATE("Update");

        /**
         * The name of the button as appears in the application
         */
        private String name;

        /**
         * Default constructor; sets the name
         *
         * @param name The string to set as the name
         */
        FormBtn(String name) {
            this.name = name;
        }

        /**
         * Gets the name associated with the button as it is displayed in the
         * Web UI
         *
         * @return The name of the button
         */
        public String getName() {
            return this.name;
        }
    }

    ///////////
    //ID's & Locators
    ///////////

    /**
     * The xpath prefix assigned to the header
     */
    protected static final String TITLE_XPATH_PREFIX = "//div[text()='";

    /**
     * The xpath prefix for the help icon
     */
    protected static final String HELP_ICON_XPATH_PREFIX = "//div[@aria-label='";

    /**
     * The xpath prefix for the close icon
     */
    protected static final String CLOSE_ICON_XPATH_PREFIX = "//div[contains(@class, 'x-title-text')][text()='";

    /**
     * The xpath suffix for the close icon
     */
    protected static final String CLOSE_ICON_XPATH_SUFFIX = "']/ancestor::div[contains(@class, 'x-header')]//div[contains(@data-qtip, 'Close')]";

    /**
     * Xpath prefix assigned to buttons in the form
     */
    protected static final String FORM_BTN_XPATH_PREFIX = "//span[@data-ref='btnInnerEl'][text()='";

    /**
     * Xpath of the OK button
     */
    protected static final String OK_BTN_XPATH = ".//span[@data-ref='btnInnerEl'][text()='OK']";

    /**
     * Xpath prefix assigned to content are in help window
     */
    protected static final String CONTENT_AREA_XPATH_PREFIX = "//div[contains(text(),'";

    /**
     * Xpath of the full Header of a window (Including header title and icons)
     */
    protected static final String HEADER_PARENT_XPATH = "/ancestor::div[contains(@class, 'x-box-inner')]";

    /**
     * Xpath to get to the full Form Header of a window from an interior element
     */
    protected static final String FORM_WINDOW_PARENT_XPATH = "/ancestor::div[contains(@class, 'x-border-box')]";

    /**
     * Xpath to the parent div of button designating status (enabled/disabled)
     */
    protected static final String PARENT_BUTTON_STATUS_DIV_SUFFIX = "./ancestor::a";

    /**
     * Xpath to dropdown options
     */
    protected static final String DROPDOWN_OPTIONS_XPATH = "//li[@role='option']";

    /**
     * Attribute of element indicating if element is enabled or disbaled
     */
    protected static final String ARIA_DISABLED_ATTRIBUTE = "aria-disabled";

    /**
     * Xpath for ancestor div
     */
    protected static final String ANCESTOR_DIV_XPATH = "/ancestor::div";

    /**
     * Attribute of an element that accesses the element's value
     */
    protected static final String VALUE_ATTRIBUTE = "value";

    ///////////
    //Getters
    ///////////

    /**
     * Gets the header element
     *
     * @param text - the text displayed in the header
     * @return the header element for the form
     */
    protected El getHeader(String text) {
        return el(By.xpath(TITLE_XPATH_PREFIX + text + "']"));
    }

    /**
     * The xpath to get from the template form content window to the help icon
     *
     * @param text -the header of the window from which the help icon is desired
     * @return the help icon
     */
    protected El getHelpIcon(String text) {
        return el(By.xpath(HELP_ICON_XPATH_PREFIX + text + " Help']"));
    }

    /**
     * Gets the close icon
     *
     * @param text -the header text where the close icon is expected
     * @return the close icon
     */
    protected El getCloseIcon(String text) {
        return el(By.xpath(CLOSE_ICON_XPATH_PREFIX + text + CLOSE_ICON_XPATH_SUFFIX));
    }

    /**
     * Gets button with given text in form with given header text
     *
     * @param btnName -the text displayed on the button in the UI
     * @param headerText -the text in the header of the window where the button
     *        is expected
     * @return the button element
     */
    protected El getFormBtn(String headerText, String btnName) {
        El btn = el(By.xpath(TITLE_XPATH_PREFIX + headerText + "']" + FORM_WINDOW_PARENT_XPATH + FORM_BTN_XPATH_PREFIX + btnName + "']"));
        return btn;
    }

    /**
     * Gets button with given text in form with given header text
     *
     * @param btnName -the text displayed on the button in the UI
     * @return the button element
     */
    protected El getFormBtn(String btnName) {
        return getFormBtn(HEADER_TEXT, btnName);
    }

    /**
     * Gets the ok button
     *
     * @param helpHeader -the header text to the help window where button is
     *        expected
     * @return the ok button
     */
    protected El getHelpOKBtn(String helpHeader) {
        El helpWindow = el(By.xpath(TITLE_XPATH_PREFIX + helpHeader + "']" + FORM_WINDOW_PARENT_XPATH));
        return helpWindow.el(By.xpath(OK_BTN_XPATH));
    }

    /**
     * Gets the content area element
     *
     * @param text - the text expected in the content area
     * @return the content area element for the form
     */
    protected El getContentArea(String text) {
        return el(By.xpath(CONTENT_AREA_XPATH_PREFIX + text + "')]"));
    }

    /**
     * Gets the options from dropdown
     *
     * @return the list of options
     */
    protected List<El> getDropdownOptions() {
        return els(By.xpath(DROPDOWN_OPTIONS_XPATH));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the header is displayed
     *
     * @param text -header text expected
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isHeaderDisplayed(String text) {
        try {
            El header = getHeader(text);
            return header != null && header.isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the help icon is displayed
     *
     * @param text -text in header where help icon is expected
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isHelpIconDisplayed(String text) {
        return isElementDisplayed(getHelpIcon(text));
    }

    /**
     * Checks to see if the close icon is displayed
     *
     * @param text -text in header where help icon is expected
     * @return true if the close icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isCloseIconDisplayed(String text) {
        return isElementDisplayed(getCloseIcon(text));
    }

    /**
     * Checks to see if the content is displayed
     *
     * @param text -text expected in the content
     * @return true if the content is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isContentDisplayed(String text) {
        return isElementDisplayed(getContentArea(text));
    }

    /**
     * Checks to see if the given button is displayed
     *
     * @param btnName -name of the button expected
     * @param headerText -the text in the header of the window where the button
     *        is expected
     * @return true if the button is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isBtnDisplayed(String headerText, String btnName) {
        return isElementDisplayed(getFormBtn(headerText, btnName));
    }

    /**
     * Checks to see if the OK button is displayed
     *
     * @param helpHeader -the header text to the help window where button is
     *        expected
     * @return true if the OK button is displayed, false if it is not or cannot
     *         be found
     */
    protected boolean isHelpOKBtnDisplayed(String helpHeader) {
        return isElementDisplayed(getHelpOKBtn(helpHeader));
    }

    /**
     * Checks to see if the given button is enabled
     *
     * @param btn -the button to check
     * @return true if the given button is enabled, false if it is disabled
     */
    public boolean isBtnEnabled(El btn) {
        String enabled = btn.getAttribute(ARIA_DISABLED_ATTRIBUTE);
        return "false".equals(enabled);
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Clicks the given form button
     *
     * @param headerText - the header text of the window in which the button is
     *        located
     * @param btnName -the name of the button to click
     */
    protected void clickFormBtn(String headerText, String btnName) {
        getFormBtn(headerText, btnName).click();
    }

    /**
     * Click the Help icon
     *
     * @param text -the header of the window from which the help icon is desired
     */
    public void clickHelpIcon(String text) {
        getHelpIcon(text).click();
    }

    /**
     * Click the Close icon
     *
     * @param text -the header of the window from which the help icon is desired
     */
    public void clickCloseIcon(String text) {
        getCloseIcon(text).click();
    }

    /**
     * Click the OK button and waits for window to close
     *
     * @param helpHeader -the header text to the help window where button is
     *        expected
     */
    protected void clickHelpOKBtn(String helpHeader) {
        getHelpOKBtn(helpHeader).click();
        waitForElementToBeInvisible(By.xpath(TITLE_XPATH_PREFIX + helpHeader + "')]"));
    }

    /**
     * Selects the given value from the given dropdown list
     *
     * @param dropdown - the dropdown to click
     * @param specificOption - the specific option expected in dropdown list
     */
    protected void selectFromDropdownList(El dropdown, El specificOption) {
        dropdown.click();
        specificOption.click();
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for the presence of the Help and Close icons
     *
     * @param text -text expected in header where help icon is located
     */
    public void checkHeaderIcons(String text) {
        Assert.assertTrue("The Help icon is not displayed.", isHelpIconDisplayed(text));
        Assert.assertTrue("The Close icon is not displayed.", isCloseIconDisplayed(text));
    }

    /**
     * Checks the help text is displayed correctly for this form's help window
     * (assumes the help window is displayed)
     */
    public void checkHelpText() {
        Assert.assertTrue("Unable to locate a content area with the expected help text: <" + HELP_TEXT + ">", this.getContentArea(HELP_TEXT).isDisplayed());
    }
}
