package com.bws.base.utils;

import com.bws.base.BusinessObject;

import java.util.*;

/**
 * Java Collections Framework utilities.
 * <p/>
 * <b>Note</b>: Please consider revising commons-collections component before adding a feature in this class.
 * 
 * @author Anton Kraievoy
 */
public class Jcf {
    /**
     * As we often use this value, I've decided to move them to constants
     * to reduce memory footprints and improve code a bit.
     */
    public static final Long LONG_ZERO = (long) 0;

    public static final Integer INTEGER_ZERO = 0;
    /**
     * As we often use this value, I've decided to move them to constants
     * to reduce memory footprints and improve code a bit.
     */
    public static final Long LONG_ONE = (long) 1;

    public static final Long LONG_MINUS_ONE = (long) -1;

    /**
     * As we often use this value, I've decided to move them to constants
     * to reduce memory footprints and improve code a bit.
     */
    public static final Long[] LONG_ARRAY_ID = new Long[0];

    public static final String[] STRING_ARRAY_ID = new String[0];
    /**
     * As we often use this value, I've decided to move them to constants
     * to reduce memory footprints and improve code a bit.
     */
    public static final Byte BYTE_ZERO = (byte) 0x00;
    /**
     * As we often use this value, I've decided to move them to constants
     * to reduce memory footprints and improve code a bit.
     */
    public static final Byte BYTE_ONE = (byte) 0x01;

    public static final Random RANDOM = new Random();

    private Jcf() {
        // utility class with no public constructor 
    }

    public static <E> Collection<E> merge(final Collection<E> collectionA, final Collection<E> collectionB) {
        final List<E> allParams = new ArrayList<E>();
        allParams.addAll(collectionA);
        allParams.addAll(collectionB);
        return allParams;
    }

    public static <E> boolean sameContent(final Collection<E>  collectionA, final Collection<E>  collectionB) {
        if (collectionA.size() != collectionB.size()) {
            return false;
        }

        final Collection collectionBLeft = new ArrayList<E>(collectionB);

        for (E anObjectFromA : collectionA) {
            if (!collectionBLeft.contains(anObjectFromA)) {
                return false;
            }

            collectionBLeft.remove(anObjectFromA);
        }

        return true;
    }

    public static <E> boolean sameContentAndOrder(final List<E>  listA, final List<E>  listB) {
        if (listA.size() != listB.size()) {
            return false;
        }

        for (int elementIndex = 0; elementIndex < listA.size(); elementIndex++) {
            E elementFromA = listA.get(elementIndex);
            E elementFromB = listB.get(elementIndex);

            if (!nullSafeEquals(elementFromA, elementFromB)) {
                return false;
            }
        }

        return true;
    }

    /**
     * Simple enhancement of equals() that performs additional null checks.
     * It assumess that null references are equal.
     */
    public static <T> boolean nullSafeEquals(final T objectA, final T objectB) {
        if (objectA == null && objectB == null) {
            return true;
        }

        if (objectA == null || objectB == null) {
            return false;
        }

        if (objectA instanceof Number && objectB instanceof Number) {
            Number numberA = (Number) objectA;
            Number numberB = (Number) objectB;

            final long aLong = numberA.longValue();
            final double aDouble = numberA.doubleValue();

            final long bLong = numberB.longValue();
            final double bDouble = numberB.doubleValue();

            if (aLong == aDouble && bLong == bDouble) {
                return aLong == bLong;
            } else {
                return aDouble == bDouble;
            }
        }
        return objectA.equals(objectB);
    }

    /**
     * Detects if array is not <code>null</code> and its length is greater than 0.
     */
    public static boolean isNotEmpty(Object[] array) {
        return !isEmpty(array);
    }

    /**
     * Detects if array is <code>null</code> or its length is 0.
     */
    public static boolean isEmpty(Object[] array) {
        return array == null || array.length <= 0;
    }

    /**
     * Detects if Collection is not <code>null</code> and its size is greater than 0.
     */
    public static boolean isNotEmpty(Collection collection) {
        return !isEmpty(collection);
    }

    /**
     * Detects if <code>Collection</code> is <code>null</code> or its size is 0.
     */
    public static boolean isEmpty(Collection collection) {
        return collection == null || collection.isEmpty();
    }

    /**
     * Returns array of <code>Long</code>s, given a <code>Collection</code> of <code>Long</code>s.
     *
     * @param longObjects must be NOT <code>null</code>
     * @return an array of <code>Long</code>s, always not <code>null</code>
     */
    public static Long[] asLongArray(final Collection<Long> longObjects) {
        return longObjects.toArray(new Long[longObjects.size()]);
    }

    public static Long[] asLongObjs(long[] longs) {
        final Long[] result = new Long[longs.length];
        for (int index = 0; index < result.length; index++) {
            result[index] = longs[index];
        }
        return result;
    }

    public static Integer[] asIntegerObjs(int[] ints) {
        final Integer[] result = new Integer[ints.length];
        for (int index = 0; index < result.length; index++) {
            result[index] = ints[index];
        }
        return result;
    }

    /**
     * Substitutes <code>null</code> <code>Long</code> array with an empty one.
     * Returns the same instance otherwise.
     */
    public static Long[] substNull(Long[] longObjects) {
        if (longObjects == null) {
            return LONG_ARRAY_ID;
        }
        return longObjects;
    }

    /**
     * Substitutes <code>null</code> List with an empty, writable one.
     * Returns the same instance otherwise.
     */
    public static List substNull(List someList) {
        if (someList == null) {
            return new ArrayList();
        }
        return someList;
    }

    public static Long[] wrapAsArray(Long longValue) {
        return new Long[] { longValue };
    }

    public static String toCommaSeparatedStringValues(Long[] values) {
        if (isNotEmpty(values)) {
            StringBuffer sBuffer = new StringBuffer();
            for (Long value : values) {
                sBuffer.append(value).append(',');
            }
            Str.removeLastComma(sBuffer);
            return sBuffer.toString();
        } else {
            return "";
        }
    }

    public static List<Long> getIdList(List<BusinessObject> listOfBusinessObjects) {
        return getIdList(toBOArray(listOfBusinessObjects));
    }

    public static Long[] getIdArray(List<BusinessObject> listOfBusinessObjects) {
        return getIdArray(toBOArray(listOfBusinessObjects));
    }

    public static Long[] getIdArray(BusinessObject[] objects) {
        if (objects ==null) {
            throw new IllegalArgumentException("JCFUtil.toListOfLongIds: arrayOfBusinessObjects is null.");
        }

        Long[] arrOfLongIds = new Long[objects.length];
        for (int objIndex = 0; objIndex < objects.length; objIndex++) {
            arrOfLongIds[objIndex] = objects[objIndex].getId();
        }

        return arrOfLongIds;
    }

    public static List<Long> getIdList(BusinessObject[] objects) {
        return Arrays.asList(getIdArray(objects));
    }

    public static Long[] toArrayOfLongIds(List arrayOfBusinessObjects) {
        if (arrayOfBusinessObjects==null) {
            throw new IllegalArgumentException("JCFUtil.toArrayOfLongIds: arrayOfBusinessObjects is null.");
        }
        Long[] arrayOfLongIds = new Long[arrayOfBusinessObjects.size()];
        Iterator iterator = arrayOfBusinessObjects.iterator();
        int i=0;
        while (iterator.hasNext()) {
              arrayOfLongIds[i++] = ((BusinessObject) iterator.next()).getId();
        }
        return arrayOfLongIds;
    }

    public static Object getRandomElement(final List elements) {
        return elements.get(RANDOM.nextInt(elements.size()));
    }

    public static boolean contains(final Enumeration keys, final String searchKey) {
        while (keys.hasMoreElements()) {
            String key = (String) keys.nextElement();
            if (nullSafeEquals(key, searchKey)) {
                return true;
            }
        }
        return false;
    }

    public static boolean contains(Long[] longObjects, final Long searchKey) {
        if (longObjects == null) {
            return false;
        }

        for (Long longObject : longObjects) {
            if (nullSafeEquals(longObject, searchKey)) {
                return true;
            }
        }

        return false;
    }

    public static boolean contains(Collection objects, final Object searchKey) {
        return objects != null && objects.contains(searchKey);
    }

    public static List list(final Object[] array) {
        return array == null ? Collections.EMPTY_LIST : Arrays.asList(array);
    }

    public static long randomLong() {
        return RANDOM.nextLong();
    }

    public static boolean isEmptyOrNull(Long element) {
        return element == null || Str.EMPTY.equals(element.toString().trim());
    }

    public static long randomPositiveLong() {
        return Math.abs(RANDOM.nextLong());
    }

    public static int randomPositiveInt() {
        return Math.abs(RANDOM.nextInt());
    }

    public static BusinessObject[] toBOArray(final Collection<BusinessObject> boCollection) {
        return boCollection.toArray(new BusinessObject[boCollection.size()]);
    }

    public static boolean isPositive(Long longObject) {
        return longObject != null && LONG_ZERO.compareTo(longObject) < 0;
    }

    public static boolean isPositive(Integer intObject) {
        return intObject != null && INTEGER_ZERO.compareTo(intObject) < 0;
    }

    public static boolean isPositiveOrMinusOne(Long longObject) {
        return longObject != null && (LONG_ZERO.compareTo(longObject) < 0 || LONG_MINUS_ONE.equals(longObject));
    }

    public static boolean sameContent(final long[] longsExpected, final long[] longsActual) {
        return sameContent(asLongObjs(longsExpected), asLongObjs(longsActual));
    }

    public static boolean sameContent(final Long[] longsExpected, final Long[] longsActual) {
        return sameContent(Arrays.asList(longsExpected), Arrays.asList(longsActual));
    }

    public static boolean sameContentAndOrder(final long[] longsExpected, final long[] longsActual) {
        return sameContentAndOrder(asLongObjs(longsExpected), asLongObjs(longsActual));
    }

    public static boolean sameContentAndOrder(final Long[] longsExpected, final Long[] longsActual) {
        return sameContentAndOrder(Arrays.asList(longsExpected), Arrays.asList(longsActual));
    }

    public static Long bool2Long(final boolean deleteCascade) {
        return deleteCascade ? LONG_ONE : LONG_ZERO;
    }

    public static int getArrayLength(Object[] array) {
        return array != null ? array.length : 0;
    }
    
    public static int parseInt(Object object) {
        if (object == null || Str.isEmpty(object)) return 0;
        
        return Integer.parseInt(object.toString());
    }

    public static <T> List<T> narrow(final List someObjects, T sample) {
        final List<T> result = new ArrayList<T>(someObjects.size());
        result.addAll(someObjects);
        return result;
    }

    /** 
     * Juts to hide cast unsafety from inspectors in production code. 
     */
    public static <T> T[] toArray(final List someObjects, T[] sample) {
        return (T[]) someObjects.toArray(sample);
    }

    public static <K, V> Map<K, V> narrow(final Map someMap, K keySample, V valueSample) {
        final Map<K, V> result = new HashMap<K, V>(someMap.size());
        result.putAll(someMap);
        return result;
    }
}

