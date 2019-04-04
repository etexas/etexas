package com.harmonia.qa.ETEXASWebQATests.PageTests;

import junit.framework.Assert;

import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;

/**
 * Test class which tests the loading of the critical elements on the landing
 * page
 *
 * @author cbulloss
 */
public class LandingPageTests extends ETexasAfterTestResetTestBase {

    /**
     * Simple test of the eTexas landing page
     */
    @Test
    public void landingPageTest() {
        LandingPage landingPage = ETexasCommonUtils.goToLandingPage();
        Assert.assertTrue("The landing page header is not displayed.", landingPage.isHeaderDisplayed());
        Assert.assertTrue("The user login form is not displayed.", landingPage.isLoginFormDisplayed());
    }
}
