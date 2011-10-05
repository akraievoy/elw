package elw.vo;

import java.util.*;
import java.util.regex.Pattern;

public class FileType implements IdNamed {
    private String id;
    public String getId() { return id; }
    public void setId(String id) { this.id = id; }

    private String name;
    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    private long lengthLimit;
    public long getLengthLimit() { return lengthLimit; }
    public void setLengthLimit(long lengthLimit) { this.lengthLimit = lengthLimit; }

    private String nameRegex;
    public String getNameRegex() { return nameRegex; }
    public void setNameRegex(String nameRegex) { this.nameRegex = nameRegex; }

    private String viewer;
    public String getViewer() { return viewer; }
    public void setViewer(String viewer) { this.viewer = viewer; }

    private String editor;
    public String getEditor() { return editor; }
    public void setEditor(String editor) { this.editor = editor; }

    private java.lang.Class validator;
    public java.lang.Class getValidator() { return validator; }
    public void setValidator(java.lang.Class validator) { this.validator = validator; }

    private boolean binary;
    public boolean isBinary() { return binary; }
    public void setBinary(boolean binary) { this.binary = binary; }

    private final List<String> contentTypes = new ArrayList<String>();
    public List<String> getContentTypes() {
        return Collections.unmodifiableList(contentTypes);
    }
    public void setContentTypes(List<String> contentTypes) {
        this.contentTypes.clear();
        if (contentTypes != null) {
            this.contentTypes.addAll(contentTypes);
        }
    }

    private final List<byte[]> headers = new ArrayList<byte[]>();
    public List<byte[]> getHeaders() {
        return Collections.unmodifiableList(headers);
    }
    public void setHeaders(List<byte[]> headers) {
        this.headers.clear();
        if (headers != null) {
            this.headers.addAll(headers);
        }
    }

    public static class _ {
        public static void filterByLength(SortedMap<String, FileType> validTypes, long length) {
            for (Iterator<String> fileTypeIds = validTypes.keySet().iterator(); fileTypeIds.hasNext();) {
                if (length > validTypes.get(fileTypeIds.next()).getLengthLimit()) {
                    fileTypeIds.remove();
                }
            }
        }

        public static void filterByName(SortedMap<String, FileType> validTypes, String fname) {
            for (Iterator<String> fileTypeIds = validTypes.keySet().iterator(); fileTypeIds.hasNext();) {
                final Pattern pattern = Pattern.compile(validTypes.get(fileTypeIds.next()).getNameRegex());
                if (!pattern.matcher(fname).matches()) {
                    fileTypeIds.remove();
                }
            }
        }
    }
}
