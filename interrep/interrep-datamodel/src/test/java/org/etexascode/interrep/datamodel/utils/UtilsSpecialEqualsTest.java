/*
 * #%L
 * eTEXAS
 * %%
 * Copyright (C) 2016 Harmonia Holdings Group, LLC
 * %%
 * All rights reserved.
 * -
 * SBIR DATA RIGHTS
 * Harmonia Holdings Group, LLC
 * 2020 Kraft Drive Suite 2400
 * Blacksburg, VA 24060
 * Contract No: DTRT57-16-c-10008
 * Start Date: 01/05/2016
 * End Date: 01/05/2018
 * Expiration of SBIR Data Rights Period: 01/05/2022
 * -
 * The Government's rights to use, modify, reproduce, release, perform,
 * display, or disclose technical data or computer software marked with
 * this legend are restricted during the period shown as provided in
 * paragraph (b)(4) of the Rights in Noncommercial Technical Data and
 * Computer Software-Small Business Innovation Research (SBIR) Program
 * clause contained in the above identified contract. No restrictions
 * apply after the expiration date shown above. Any reproduction of
 * technical data, computer software, or portions thereof marked with
 * this legend must also reproduce the markings.
 * -
 * Contributors:
 * Harmonia Holdings Group LLC: Initial API and implementation.
 * #L%
 */

package org.etexascode.interrep.datamodel.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.awt.Polygon;
import java.util.ArrayList;
import java.util.List;

import org.etexascode.interrep.datamodel.interfaces.IDable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author bmauldon
 * @author ablatt
 */
public class UtilsSpecialEqualsTest {

    private final int[] x1 = { 20, 40, 40, 60, 60, 40 };

    private final int[] y1 = { 20, 20, 40, 40, 60, 20 };

    Polygon p1 = new Polygon(x1, y1, 6);

    private final int[] x2 = { 20, 40, 40, 60, 60, 40 };

    private final int[] y2 = { 20, 20, 40, 40, 60, 20 };

    Polygon p2 = new Polygon(x2, y2, 6);

    private final int[] x3 = { 20, 30, 40, 50, 60, 60, 60 };

    private final int[] y3 = { 20, 30, 40, 50, 40, 50, 60 };

    Polygon p3 = new Polygon(x3, y3, 7);

    private final int[] empty = {};

    Polygon p = new Polygon(empty, empty, 0);

    private final int[] x4 = { 1, 2, 3 };

    private final int[] y4 = { 1, 2, 3 };

    Polygon p4 = new Polygon(x4, y4, 3);

    private final int[] x5 = { 4, 5, 6 };

    private final int[] y5 = { 4, 5, 6 };

    Polygon p5 = new Polygon(x5, y5, 3);

    private final int[] x6 = { 2, 2, 3 };

    private final int[] y6 = { 2, 2, 3 };

    Polygon p6 = new Polygon(x6, y6, 3);

    private final int[] x7 = { 3, 2, 1 };

    private final int[] y7 = { 3, 2, 1 };

    Polygon p7 = new Polygon(x7, y7, 3);

    private final int x11 = 10;

    private final int y11 = 20;

    private final int x12 = 10;

    private final int y12 = 20;

    private final int x13 = 10;

    private final int y13 = 30;

    private final int x14 = 20;

    private final int y14 = 30;

    IDImple idimple = null;

    String actualId = null;

    List<IDImple> idContTest1 = null;

    List<IDImple> idContTest2 = null;

    List<IDImple> idContTest3 = null;

    @Before
    public void setUp() throws Exception {
        idimple = new IDImple();
        actualId = "randId";
        idimple.id = actualId;
        idContTest1 = genIdableList1();
        idContTest2 = genIdableList2();
        idContTest3 = genIdableList3();
    }

    @After
    public void tearDown() throws Exception {
        idimple = null;
        actualId = null;
        idContTest1 = null;
        idContTest2 = null;
        idContTest3 = null;
    }

    @Test
    public void testEqualsPolygonPolygon() {
        assertTrue(UtilsSpecialEquals.equals(p1, p2));
        assertFalse(UtilsSpecialEquals.equals(p1, p3));

        assertTrue(UtilsSpecialEquals.equals(p, p));
        assertFalse(UtilsSpecialEquals.equals(p4, p5));
        assertFalse(UtilsSpecialEquals.equals(p4, p6));
        assertTrue(UtilsSpecialEquals.equals(p4, p7));
    }

    @Test
    public void testPointEquals() {
        assertTrue(UtilsSpecialEquals.pointEquals(x11, y11, x12, y12));
        assertFalse(UtilsSpecialEquals.pointEquals(x11, y11, x13, y13));
        assertFalse(UtilsSpecialEquals.pointEquals(x11, y11, x14, y14));
    }

    @Test
    public void testConstructor() {
        UtilsSpecialEquals obj = new UtilsSpecialEquals();
    }

    @Test
    public void testIdableConatins1() {
        assertTrue(UtilsSpecialEquals.idableConatins(idContTest1, idimple));
    }

    @Test
    public void testIdableConatins2() {
        assertTrue(UtilsSpecialEquals.idableConatins(idContTest2, idimple));
    }

    @Test
    public void testIdableConatins3() {
        assertFalse(UtilsSpecialEquals.idableConatins(idContTest3, idimple));
    }

    @Test
    public void testIdableConatins4() {
        assertFalse(UtilsSpecialEquals.idableConatins(null, idimple));
    }

    @Test
    public void testIdableConatins5() {
        assertFalse(UtilsSpecialEquals.idableConatins(idContTest1, null));
    }

    class IDImple implements IDable {

        String id;

        @Override
        public boolean equalsId(IDable entity) {
            return id.equals(entity.getProperId());
        }

        @Override
        public String getProperId() {
            return id;
        }
    }

    private List<IDImple> genIdableList1() {
        ArrayList<IDImple> ret = new ArrayList<IDImple>();

        IDImple id = new IDImple();
        id.id = "something else 1";
        ret.add(id);

        id = new IDImple();
        id.id = "something else 2";
        ret.add(id);

        ret.add(idimple);

        id = new IDImple();
        id.id = "something else 3";
        ret.add(id);

        id = new IDImple();
        id.id = "something else 4";
        ret.add(id);

        return ret;
    }

    private List<IDImple> genIdableList2() {
        ArrayList<IDImple> ret = new ArrayList<IDImple>();

        IDImple id = new IDImple();
        id.id = "something else 1";
        ret.add(id);

        id = new IDImple();
        id.id = "something else 2";
        ret.add(id);

        ret.add(idimple);

        id = new IDImple();
        id.id = "something else 3";
        ret.add(id);

        ret.add(idimple);

        return ret;
    }

    private List<IDImple> genIdableList3() {
        ArrayList<IDImple> ret = new ArrayList<IDImple>();

        IDImple id = new IDImple();
        id.id = "something else 1";
        ret.add(id);

        id = new IDImple();
        id.id = "something else 2";
        ret.add(id);

        id = new IDImple();
        id.id = "something else 5";
        ret.add(id);

        id = new IDImple();
        id.id = "something else 3";
        ret.add(id);

        id = new IDImple();
        id.id = "something else 4";
        ret.add(id);

        return ret;
    }
}
