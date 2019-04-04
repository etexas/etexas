package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.ETexasBasePage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the pop-up window which displays after successful
 * registration
 *
 * @author cbulloss
 */
public class RegisterSuccessWindow extends ETexasBasePage {

    /**
     * ID assigned to the window header
     */
    public static final String HEADER_ID = "messagebox-1001_header";

    /**
     * ID assigned to the exit icon
     */
    public static final String EXIT_ICON_ID = "tool-1030-toolEl";

    /**
     * Xpath to the ok button
     */
    public static final String OK_BTN_XPATH = "//span[text()='OK']";

    /**
     * ID assigned to the div which contains the window's content (e.g. "Welcome
     * to the eTEXAS...")
     */
    public static final String CONTENT_DIV_ID = "messagebox-1001-displayfield-inputEl";

    /**
     * Default constructor
     *
     * @param driver the web driver instance being used
     */
    public RegisterSuccessWindow(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    ///////////
    //Getters
    ///////////

    /**
     * Gets the window header
     *
     * @return the window header
     */
    private El getHeader() {
        return el(By.id(HEADER_ID));
    }

    /**
     * Gets the content div
     *
     * @return the div where window content is displayed
     */
    private El getWindowContent() {
        return el(By.id(CONTENT_DIV_ID));
    }

    /**
     * Gets the Register button
     *
     * @return the Ok button
     */
    private El getOkBtn() {
        return el(By.xpath(OK_BTN_XPATH));
    }

    /**
     * Gets the exit icon for this window
     *
     * @return the exit icon
     */
    private El getExitIcon() {
        return el(By.id(EXIT_ICON_ID));
    }

    ///////////
    //Checkers
    ///////////

    /**
     * Checks to see if the pop-up window's header is displayed
     *
     * @return true if the header is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isHeaderDisplayed() {
        try {
            return getHeader().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the window content div is displayed. Does not check the
     * value or length of any text displayed. Presence of this div does not
     * guarantee text displayed
     *
     * @return true if the content div is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isContentDisplayed() {
        try {
            return getWindowContent().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the Register button is displayed
     *
     * @return true if the Register button is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isRegisterBtnDisplayed() {
        try {
            return getOkBtn().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks to see if the Exit icon is displayed
     *
     * @return true if the icon is displayed, false if it is not or cannot be
     *         found
     */
    public boolean isExitIconDisplayed() {
        try {
            return getExitIcon().isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    ///////////
    //Interaction
    ///////////

    /**
     * Clicks the Ok button (dismisses the popup)
     */
    public void clickOk() {
        getOkBtn().click();
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(OK_BTN_XPATH), 30);
    }

}