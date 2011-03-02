/*
 * ELW : e-learning workspace
 * Copyright (C) 2010  Anton Kraievoy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package elw.dp.app;

import junit.framework.TestCase;

import java.io.ByteArrayInputStream;
import java.io.IOException;

public class RunnableLoadTaskTest extends TestCase {
	protected static final String RESP_LIST_JSON =
			"{\n" +
			"  \"message\" : null,\n" +
			"  \"data\" : {\n" +
			"    \"statement\" : {\n" +
			"      \"v\" : [ \"gepq8ks1\" ],\n" +
			"      \"t\" : [ ],\n" +
			"      \"s\" : [ ],\n" +
			"      \"a\" : [ ]\n" +
			"    },\n" +
			"    \"test\" : {\n" +
			"      \"v\" : [ \"gepq8ks6\", \"gepq8ksa\", \"gepq8kse\" ],\n" +
			"      \"t\" : [ ],\n" +
			"      \"s\" : [ ],\n" +
			"      \"a\" : [ ]\n" +
			"    },\n" +
			"    \"report\" : {\n" +
			"      \"v\" : [ ],\n" +
			"      \"t\" : [ \"gfp5746n\", \"gfpakao2\" ],\n" +
			"      \"s\" : [ \"gk8auy9y\", \"gknupkbn\" ],\n" +
			"      \"a\" : [ ]\n" +
			"    },\n" +
			"    \"code\" : {\n" +
			"      \"v\" : [ ],\n" +
			"      \"t\" : [ ],\n" +
			"      \"s\" : [ \"gk8a6aj4\", \"gknu3opt\", \"gknxekr2\" ],\n" +
			"      \"a\" : [ ]\n" +
			"    },\n" +
			"    \"reference\" : {\n" +
			"      \"v\" : [ \"gepq8ksj\" ],\n" +
			"      \"t\" : [ ],\n" +
			"      \"s\" : [ ],\n" +
			"      \"a\" : [ ]\n" +
			"    }\n" +
			"  },\n" +
			"  \"success\" : true\n" +
			"}";

	public void testReadRespList() throws IOException {
		final ResponseList respList = RunnableLoadTask.readRespList(
				new ByteArrayInputStream(RESP_LIST_JSON.getBytes("UTF-8"))
		);
		assertNotNull(respList);
		assertEquals("gknu3opt", respList.getData().get("code").get("s")[1]);
	}
}
