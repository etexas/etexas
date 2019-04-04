package com.harmonia.qa.ETEXASWebQATests.webdriver.bases;

import org.junit.Rule;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 * Class to reset test base for ETexas tests
 *
 * @author llaroussini
 */
public class ETexasAfterTestResetTestBase extends ETexasTestBase {

    /**
     * Rule chain that wraps the screenshot rule in an outer rule that quits the
     * webdriver. The screenshot test rule will evaluate the test statement
     * first, then quit
     */
    @Rule
    public RuleChain afterTestRuleChain = RuleChain.outerRule(new TestRule() {

        @Override
        public Statement apply(final Statement base, Description description) {
            return new Statement() {

                @Override
                public void evaluate() throws Throwable {
                    try {
                        base.evaluate();
                    }
                    finally {
                        quit();
                    }
                }
            };
        }
    }).around(screenshotRule);

    /**
     * Default constructor
     */
    public ETexasAfterTestResetTestBase() {
        super();
    }

}
