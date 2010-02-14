package elw.dp.mips.asm;

import junit.framework.TestCase;

public class DataTest extends TestCase {
	public void testIsValidInt() {
		assertTrue(Data.isNum("0", 32));
		assertTrue(Data.isNum("0x0", 32));
		assertTrue(Data.isNum("0b", 32));
		assertTrue(Data.isNum("0h", 32));
		assertTrue(Data.isNum("0o", 32));
		assertTrue(Data.isNum("9", 32));
		assertTrue(Data.isNum("9h", 32));
		assertTrue(Data.isNum("0x9", 32));
		assertTrue(Data.isNum("7o", 32));
		assertTrue(Data.isNum("1o", 32));
		assertTrue(Data.isNum("1b", 32));

		assertTrue(Data.isNum("-0", 32));
		assertTrue(Data.isNum("-0x0", 32));
		assertTrue(Data.isNum("-0b", 32));
		assertTrue(Data.isNum("-0h", 32));
		assertTrue(Data.isNum("-0o", 32));
		assertTrue(Data.isNum("-9", 32));
		assertTrue(Data.isNum("-9h", 32));
		assertTrue(Data.isNum("-0x9", 32));
		assertTrue(Data.isNum("-7o", 32));
		assertTrue(Data.isNum("-1o", 32));
		assertTrue(Data.isNum("-1b", 32));

		assertFalse(Data.isNum("-F", 32));
		assertFalse(Data.isNum("-0xG", 32));
		assertFalse(Data.isNum("-2b", 32));
		assertFalse(Data.isNum("-Gh", 32));
		assertFalse(Data.isNum("-8o", 32));
		assertFalse(Data.isNum("-9-", 32));
		assertFalse(Data.isNum("9-h", 32));
		assertFalse(Data.isNum("-00x9", 32));
		assertFalse(Data.isNum("-8o", 32));
		assertFalse(Data.isNum("-8o", 32));
		assertFalse(Data.isNum("-2b", 32));

		assertFalse(Data.isNum(null, 32));
		assertFalse(Data.isNum("-", 32));
		assertFalse(Data.isNum("", 32));
		assertFalse(Data.isNum("--", 32));
		assertFalse(Data.isNum("0x", 32));
		assertFalse(Data.isNum("h", 32));
		assertFalse(Data.isNum("b", 32));
		assertFalse(Data.isNum("o", 32));
	}

	public void testParseInt() {
		assertEquals(0, Data.parse("-0"));
		assertEquals(0, Data.parse("-0x0"));
		assertEquals(0, Data.parse("-0b"));
		assertEquals(0, Data.parse("-0h"));
		assertEquals(0, Data.parse("-0o"));
		assertEquals(-9, Data.parse("-9"));
		assertEquals(-9, Data.parse("-9h"));
		assertEquals(-9, Data.parse("-0x9"));
		assertEquals(-7, Data.parse("-7o"));
		assertEquals(-1, Data.parse("-1o"));
		assertEquals(-1, Data.parse("-1b"));

		assertEquals(-10, Data.parse("-10"));
		assertEquals(-16, Data.parse("-0x10"));
		assertEquals(-2, Data.parse("-10b"));
		assertEquals(-16, Data.parse("-10h"));
		assertEquals(-8, Data.parse("-10o"));
		assertEquals(-19, Data.parse("-19"));
		assertEquals(-25, Data.parse("-19h"));
		assertEquals(-25, Data.parse("-0x19"));
		assertEquals(-15, Data.parse("-17o"));
		assertEquals(-9, Data.parse("-11o"));
		assertEquals(-3, Data.parse("-11b"));
	}

	public void testMsb() {
		assertEquals(0, Data.width(0));
		assertEquals(1, Data.width(1));
		assertEquals(2, Data.width(2));
		assertEquals(2, Data.width(3));
		assertEquals(3, Data.width(4));
		assertEquals(4, Data.width(12));
		assertEquals(5, Data.width(25));
		assertEquals(10, Data.width(1023));
		assertEquals(11, Data.width(1025));
		assertEquals(11, Data.width(1025));
		assertEquals(26, Data.width((1 << 25) + (1 << 23) + (1 << 12) + (1 << 3 + 1)));
	}
}
