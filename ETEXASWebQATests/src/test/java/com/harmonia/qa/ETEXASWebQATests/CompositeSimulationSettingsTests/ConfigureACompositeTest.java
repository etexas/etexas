package com.harmonia.qa.ETEXASWebQATests.CompositeSimulationSettingsTests;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.ETexasUserFactory;
import com.harmonia.qa.ETEXASWebQATests.entities.factories.SimulationFactory;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasEntityManager;
import com.harmonia.qa.ETEXASWebQATests.utilities.simulations.ETexasSimulationUtils;
import com.harmonia.qa.ETEXASWebQATests.utilities.user.ETexasUserUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasAfterTestResetTestBase;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage.EditOptions;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage.SimBtns;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;

/**
 * Executes steps for Configure A Composite test case, TC-098
 *
 * @author llaroussini
 */
public class ConfigureACompositeTest extends ETexasAfterTestResetTestBase {

    /**
     * User object used throughout the test case
     */
    private ETexasUser testUser;

    /**
     * Composite object used throughout the test case
     */
    private CompositeSimulation composite;

    /**
     * Simulation object used throughout the test case
     */
    private TemplateSimulation simulation;

    /**
     * Test setup.
     */
    @Before
    public void warmUp() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - Warm Up");

        //Get  user and simulation/composite
        testUser = ETexasUserFactory.getUser(true); //Get a random user.
        simulation = SimulationFactory.getTemplateSimulation(testUser, true); //get a random template simulation
        composite = simulation.getComposite();
        ETexasEntityManager.addEntities(testUser, simulation, composite);

        //Register user and create simulation/composite
        LandingPage landing = ETexasUserUtils.userRegistration(testUser);
        ETexasSimulationUtils.createTemplateSimulation(simulation);

        //User is logged in
        landing.loginAs(testUser);
    }

    /**
     * Test steps for TC-098
     */
    @Test
    public void configureCompositeExternalTest() {
        //Set screenshot
        screenshotRule.setFileName(this.getClass().getSimpleName() + " - External Test Case");

        //Ensure the Simulations page is loaded
        SimulationsPage simPage = getPage(SimulationsPage.class);

        //Select a composite that has no executions.
        simPage.selectComposite(composite, true);

        //Verify the Edit button is enabled.
        Assert.assertTrue("Edit button is not enabled as expected when composite is selected.", simPage.isBtnEnabled(SimBtns.EDIT_BTN));

        //Click the Edit button.
        simPage.clickEdit();

        //Verify the Composite Settings option is enabled.
        Assert.assertTrue("Composite Settings option is not enabled as expected in Edit menu when composite is selected.", simPage.isEditOptionEnabled(EditOptions.COMPOSITE_SETTINGS));

        //Click the Composite Settings option.
        CompositeSettingsModal compositeSettingsModal = simPage.clickCompositeSettings();

        //Verify the Composite Settings modal is displayed.
        Assert.assertTrue("Composite Setting modal is not displayed as expected after clicking Composite Settings in Edit menu.", compositeSettingsModal.isCompositeSettingsHeaderDisplayed());

        //Verify the following icons are displayed in the top right corner of the modal: ‘?’ and ‘x’.
        compositeSettingsModal.checkHeaderIcons();

        //Click the ‘?’ icon.
        compositeSettingsModal.clickCompositeSettingsHelp();

        //Verify a Composite Settings Help modal is displayed with instructions for configuring composites.
        compositeSettingsModal.checkHelpModal();

        //Verify an OK button is displayed.
        Assert.assertTrue("OK button is not displayed as expected in Composite Settings Help modal.", compositeSettingsModal.isHelpOKBtnDisplayed());

        //Click the OK button.
        compositeSettingsModal.clickHelpOKBtn();

        //Verify the Help modal is no longer displayed.
        Assert.assertFalse("Composite Settings Help modal is still displayed after clicking OK.", compositeSettingsModal.isCompositeSettingsHelpHeaderDisplayed());

        //Verify the following tabs are displayed in the Composite Settings modal: Cellular Device Profiles, OBU Device Profiles, and Options.
        compositeSettingsModal.checkTabs();

        //Verify a Close button is displayed at the bottom of the modal.
        Assert.assertTrue("Close button is not displayed as expected in Composite Settings modal.", compositeSettingsModal.isCloseBtnDisplayed());

        //Click the Close button.
        compositeSettingsModal.clickCloseBtn();

        //Verify the Composite Settings modal is no longer displayed.
        Assert.assertFalse("Composite Setting modal is still displayed after clicking Close button.", compositeSettingsModal.isCompositeSettingsHeaderDisplayed());

        //Select a composite that has no executions.
        simPage.selectComposite(composite, true);

        //Click the Edit button.
        simPage.clickEdit();

        //Click the Composite Settings option.
        simPage.clickCompositeSettings();

        //Verify the Composite Settings modal is displayed.
        Assert.assertTrue("Composite Setting modal is not displayed as expected after clicking Composite Settings in Edit menu.", compositeSettingsModal.isCompositeSettingsHeaderDisplayed());

        //Click the ‘x’ icon.
        compositeSettingsModal.clickCloseIcon();

        //Verify the Composite Settings modal is no longer displayed.
        Assert.assertFalse("Composite Setting modal is still displayed after clicking 'x' icon.", compositeSettingsModal.isCompositeSettingsHeaderDisplayed());

        //Logout
        simPage.logout(testUser);
    }
}
