package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.ETexasBasePage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Menu displayed when filtering a table
 *
 * @author llaroussini
 */
public class FilterOptionsMenu extends ETexasBasePage {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public FilterOptionsMenu(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Items listed in Header Menu
     *
     * @author llaroussini
     */
    public enum HeaderMenuOption {
        /**
         * The Sort Ascending item
         */
        SORT_ASCENDING("Sort Ascending"),
        /**
         * The Sort Descending item
         */
        SORT_DESCENDING("Sort Descending"),
        /**
         * The Columns item
         */
        COLUMNS("Columns"),
        /**
         * The Filters item
         */
        FILTERS("Filters");

        /**
         * The label of the menu items as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        HeaderMenuOption(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the menu item as it is displayed in
         * the Web UI
         *
         * @return The label of the option
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath to the Filter menu
     */
    private static final String FILTER_MENU_XPATH = "//div[contains(@id, 'menu')][contains(@class, 'x-border-box')][not(contains(@style, 'hidden;'))]";

    /**
     * Xpath prefix to options listed in filter menu
     */
    private static final String FILTER_OPTION_XPATH_PREFIX = FILTER_MENU_XPATH + "//span[text()='";

    /**
     * Xpath to id check box listed in menu/sub-menu
     */
    private static final String CHECK_BOX_ID_XPATH = "//div[contains(@id, 'menu')]//span[text()='Id']//ancestor::a";

    /**
     * Xpath to type check box listed in menu/sub-menu
     */
    private static final String CHECK_BOX_TYPE_XPATH = "//div[contains(@id, 'menu')]//span[text()='Type']//ancestor::a";

    /**
     * Xpath to actions check box listed in menu/sub-menu
     */
    private static final String CHECK_BOX_ACTIONS_XPATH = "//div[contains(@id, 'menu')]//i[text()='Actions']//ancestor::a";

    /**
     * Xpath to filters text box
     */
    private static final String FILTERS_TEXT_BOX_XPATH = "//label[contains(@id, 'textfield')][contains(@class, 'x-grid-filters-find')]//ancestor::div//input";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the Filter menu
     *
     * @return the menu element
     */
    private El getFilterMenu() {
        return el(By.xpath(FILTER_MENU_XPATH));
    }

    /**
     * Gets given filter option from Filter menu
     *
     * @param item -the item to get
     * @return the filter option element
     */
    private El getFilterOption(HeaderMenuOption item) {
        return el(By.xpath(FILTER_OPTION_XPATH_PREFIX + item.label + "']"));
    }

    /**
     * Gets the id check box option from Filter menu
     *
     * @return the filter check box option element
     */
    private El getFilterCheckBoxIdOption() {
        return el(By.xpath(CHECK_BOX_ID_XPATH));
    }

    /**
     * Gets the type check box option from Filter menu
     *
     * @return the filter check box option element
     */
    private El getFilterCheckBoxTypeOption() {
        return el(By.xpath(CHECK_BOX_TYPE_XPATH));
    }

    /**
     * Gets the actions check box option from Filter menu
     *
     * @return the filter check box option element
     */
    private El getFilterCheckBoxActionsOption() {
        return el(By.xpath(CHECK_BOX_ACTIONS_XPATH));
    }

    /**
     * Gets the Filter text box
     *
     * @return the text box element
     */
    private El getFilterTextBox() {
        return el(By.xpath(FILTERS_TEXT_BOX_XPATH));
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks if Filter menu is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isFilterMenuDisplayed() {
        return isElementDisplayed(getFilterMenu());
    }

    /**
     * Checks if filter option is displayed
     *
     * @param item -the filter option item expected
     * @return true if displayed, false otherwise
     */
    public boolean isFilterOptionDisplayed(HeaderMenuOption item) {
        return isElementDisplayed(getFilterOption(item));
    }

    /**
     * Checks if id filter check box option is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isFilterCheckBoxIdOptionDisplayed() {
        return isElementDisplayed(getFilterCheckBoxIdOption());
    }

    /**
     * Checks if type filter check box option is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isFilterCheckBoxTypeOptionDisplayed() {
        return isElementDisplayed(getFilterCheckBoxTypeOption());
    }

    /**
     * Checks if actions filter check box option is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isFilterCheckBoxActionsOptionDisplayed() {
        return isElementDisplayed(getFilterCheckBoxActionsOption());
    }

    /**
     * Checks if filter text box is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isFilterTextBoxDisplayed() {
        return isElementDisplayed(getFilterTextBox());
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the given Filter menu option
     *
     * @param item -the item in the menu to click
     */
    public void clickFilterOption(HeaderMenuOption item) {
        getFilterOption(item).click();
    }

    /**
     * Hovers over the given Filter menu option
     *
     * @param item -the item in the menu to hover over
     */
    public void hoverOverFilterOption(HeaderMenuOption item) {
        getFilterOption(item).hover();
    }

    /**
     * Clicks the Type Message-specific filter check box option (listed in
     * columns sub-menu)
     */
    public void clickMessageTypeFilterCheckBox() {
        getFilterCheckBoxTypeOption().click();
    }

    /**
     * Clicks the Id Message-specific filter check box option (listed in columns
     * sub-menu)
     */
    public void clickMessageIdFilterCheckBox() {
        getFilterCheckBoxIdOption().click();
    }

    /**
     * Clicks the Actions Message-specific filter check box option (listed in
     * columns sub-menu)
     */
    public void clickMessageActionsFilterCheckBox() {
        getFilterCheckBoxActionsOption().click();
    }

    /**
     * Sets given text in filter text box
     *
     * @param text -the text to set
     */
    public void setFilterText(String text) {
        getFilterTextBox().setText(text + Keys.RETURN);
        ETexasCommonUtils.sleep(3000); //ZZZ - allows time for results to filter
    }

    /**
     * Clears text in filter text box
     */
    public void clearFilterText() {
        El filterBox = getFilterTextBox();
        filterBox.sendKeys(Keys.CONTROL + "a");
        filterBox.sendKeys(Keys.DELETE);
        ETexasCommonUtils.sleep(3000); //ZZZ - allows time for results to filter
    }

    ///////////
    // Utilities
    ///////////

    /**
     * Checks for all filter options listed in main filter menu (Sort Ascending,
     * Sort Descending, Columns, and Filters)
     */
    public void checkAllFilterOptions() {
        Assert.assertTrue("Sort Ascending option not displayed as expected.", isFilterOptionDisplayed(HeaderMenuOption.SORT_ASCENDING));
        Assert.assertTrue("Sort Descending option not displayed as expected.", isFilterOptionDisplayed(HeaderMenuOption.SORT_DESCENDING));
        Assert.assertTrue("Columns option not displayed as expected.", isFilterOptionDisplayed(HeaderMenuOption.COLUMNS));
        Assert.assertTrue("Actions option not displayed as expected.", isFilterOptionDisplayed(HeaderMenuOption.FILTERS));
    }

    /**
     * Checks for presence of all check box options specific to the Messages
     * filter menu (Id, Type, and Actions)
     */
    public void checkAllMessagesFilterCheckBoxOptions() {
        Assert.assertTrue("Id check box option not displayed as expected.", isFilterCheckBoxIdOptionDisplayed());
        Assert.assertTrue("Type check box option not displayed as expected.", isFilterCheckBoxTypeOptionDisplayed());
        Assert.assertTrue("Actions check box option not displayed as expected.", isFilterCheckBoxActionsOptionDisplayed());
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(FILTER_MENU_XPATH));
    }

    /**
     * Waits for filter menu to no longer be visisble
     */
    public void waitForFilterMenuToClose() {
        waitForElementToBeInvisible(By.xpath(FILTER_MENU_XPATH));
    }
}
