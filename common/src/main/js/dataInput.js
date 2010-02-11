function isHexDigit(c) {
  if ((("0" <= c) && (c <= "9")) ||
      (("A" <= c) && (c <= "F")))
    return true;
  else
    return false;
}

function isAddress(address) {
  for (var i = 0; i < address.length; i++)
    if (!isHexDigit(address.charAt(i))) return false;
  return true;
}

function valueOf(c) {
  if (("0" <= c) && (c <= "9")) return (c - "0");
  switch (c) {
    case "A" : return 10;
    case "B" : return 11;
    case "C" : return 12;
    case "D" : return 13;
    case "E" : return 14;
    case "F" : return 15;
  }
}

function hexStringToUnsigned(address) {
  var value = 0;
  for (var i = 0; i < address.length; i++)
    value = 16 * value + valueOf(address.charAt(i));
  return value;
}

function unsignedToHexString(number, numberOfDigits) {
  var result = "";
  var intermediateValue = number;
  var digitValue;
  var digitCount = 0;
  var temp;
  while (true) {
    digitValue = (intermediateValue % 16);
    switch (digitValue) {
      case  0: result = "0" + result;
        break;
      case  1: result = "1" + result;
        break;
      case  2: result = "2" + result;
        break;
      case  3: result = "3" + result;
        break;
      case  4: result = "4" + result;
        break;
      case  5: result = "5" + result;
        break;
      case  6: result = "6" + result;
        break;
      case  7: result = "7" + result;
        break;
      case  8: result = "8" + result;
        break;
      case  9: result = "9" + result;
        break;
      case 10: result = "A" + result;
        break;
      case 11: result = "B" + result;
        break;
      case 12: result = "C" + result;
        break;
      case 13: result = "D" + result;
        break;
      case 14: result = "E" + result;
        break;
      case 15: result = "F" + result;
        break;
    }

    digitCount++;
    temp = "" + (intermediateValue /= 16);
    if (temp.indexOf(".") == 0)
      intermediateValue = 0;
    else
      if (temp.indexOf(".") == -1) {
        temp = temp.substr(0);
        intermediateValue = Number(temp);
      }
      else {
        temp = temp.substr(0, temp.indexOf("."));
        intermediateValue = Number(temp);
      }
    if ((intermediateValue == 0) || (digitCount == numberOfDigits)) break;
  }

  while (result.length < numberOfDigits)
    result = "0" + result;
  return result;
}

function load(data)
{
  var dataString = data;
  var temp = new Array();
  var temp2 = new Array();
  var index = 0;
  var word;
  var address;
  var addressValue;

  while (temp.length > 0)
    temp.pop();
  while (temp2.length > 0)
    temp2.pop();

  if (top.getBrowserName() == "Microsoft Internet Explorer")
    temp = data.split(/\n/);    // For IE: split on newline
  else {                           // For Netscape: replace \n with space\n and split on \n
    var dataString = "";
    for (i = 0; i < data.length; i++)
      if (data.charAt(i) == "\n")
        dataString = dataString + " \n";
      else
        dataString = dataString + data.charAt(i)
    temp = dataString.split(/\n/);
  }

  for (i = 0; i < temp.length; i++) {
    temp[i] = temp[i].substr(0, temp[i].length - 1);
    if (temp[i].length > 0) {
      temp2.push(temp[i]);
    }
  }

  for (i = 0; i < temp2.length; i++)
    temp2[i] = temp2[i].toUpperCase();

  for (var i = 0; i < temp2.length; i++) {
    index = temp2[i].indexOf(":");
    if ((index == -1) || (index == 0)) {
      alert("ERROR: Data item " + temp2[i] +
            "\nMust be given with an address" +
            "\nData Loading Terminated!");
      return;
    }

    var address = temp2[i].substr(0, index);
    if (!isAddress(address)) {
      alert("ERROR: Address " + address + "\nmust be an hexadecimal number.\nData Loading Terminated!");
      return;
    }

    var addressValue = hexStringToUnsigned(address);
    if ((addressValue > 400) || ((addressValue % 4) != 0)) {
      alert("ERROR: Address " + address + "\nmust be a multiple of 4 and no more than 400.\nData Loading Terminated!");
      return;
    }
  }

  for (var i = 0; i < temp2.length; i++) {
    index = temp2[i].indexOf(":");
    word = temp2[i].substr(index + 1);
    if (word.length != 8) {
      alert("ERROR: " + word +
            "\nMust have 8 hex digits!!\nData Loading Terminated!");
      return;
    }
    for (var j = 0; j < word.length; j++) {
      var digit = word.charAt(j);
      if (((digit < "0") || (digit > "9")) &&
          ((digit < "A") || (digit > "F"))) {
        alert("ERROR: " + word +
              "\nHas an ILLEGAL character (not a Hex digit)!!\nData Loading Terminated!");
        return;
      }
    }
  }

  top.DataPath.resetData();

  while (this.dataValues.length > 0)
    this.dataValues.pop();

  for (var i = 0; i < temp2.length; i++) {
    index = temp2[i].indexOf(":");
    var arrayIndex = Math.floor((hexStringToUnsigned(temp2[i].substr(0, index))) / 4);
    if (arrayIndex > this.maxArrayIndex)
      this.maxArrayIndex = arrayIndex;
  }

  for (var i = 0; i <= this.maxArrayIndex; i++)
    this.dataValues[i] = "00000000";

  for (var i = 0; i < temp2.length; i++) {
    index = temp2[i].indexOf(":");
    var address = temp2[i].substr(0, index);
    word = temp2[i].substr(index + 1);
    var memoryIndex = Math.floor(hexStringToUnsigned(address) / 4);
    top.DataPath.setData(word, address);
    this.dataValues[memoryIndex] = word;
  }

  for (var i = 0; i < this.dataValues.length; i++) {
    this.lastLoaded[i] = this.dataValues[i];
  }

  this.showDataMemory(0, "");
  alert("Data Successfully Loaded!");
}

function reLoad() {
  dataList.reLoad();
}

function listReload() {
  top.DataPath.resetData();

  while (this.dataValues.length > 0)
    this.dataValues.pop();

  for (var i = 0; i <= this.maxArrayIndex; i++)
    this.dataValues[i] = "00000000";

  for (var i = 0; i < this.lastLoaded.length; i++) {
    this.dataValues[i] = this.lastLoaded[i];
    top.DataPath.setData(this.lastLoaded[i], unsignedToHexString(i * 4, 3));
  }
  this.showDataMemory(0, "");
}

function setDataValue(value, arrayIndex) {
  if (arrayIndex > this.maxArrayIndex) {
    for (var i = this.maxArrayIndex + 1; i <= arrayIndex; i++)
      this.dataValues[i] = "00000000";
    this.maxArrayIndex = arrayIndex;
  }
  this.dataValues[arrayIndex] = value;
}

function returnedDataAccess(value, address, access) {
  var addressString = new String(address);
  var addressValue = hexStringToUnsigned(addressString);
  var arrayIndex = Math.floor(addressValue / 4);
  var accessString = new String(access);

  if (accessString.toUpperCase() == "WRITE") {
    dataList.setDataValue(value, arrayIndex);
    dataList.showDataMemory(arrayIndex, "write");
  }
  else
    dataList.showDataMemory(arrayIndex, "read");
}

function showDataMemory(arrayIndex, access) {
  var addressValue;
  parent.DataMemory.document.open();
  parent.DataMemory.document.write("<head>");
  parent.DataMemory.document.write("<LINK HREF='pathsim.css' REL='stylesheet' type='text/css'>");
  parent.DataMemory.document.write("<div class='boxHead'>Data Memory</div>");
  parent.DataMemory.document.write("<div class ='code'>");
  if (access.length == 0) {
    for (var i = 0; i < this.dataValues.length; i++)
      if (this.dataValues[i] != "00000000")
        parent.DataMemory.document.write(unsignedToHexString(i * 4, 3) + ":" + this.dataValues[i] + "<br>");
  }
  else {
    for (var i = 0; i < arrayIndex; i++) {
      if (this.dataValues[i] != "00000000")
        parent.DataMemory.document.write(unsignedToHexString(i * 4, 3) + ":" + this.dataValues[i] + "<br>");
    }
    parent.DataMemory.document.write("</div>");

    if (access.toUpperCase() == "WRITE")
      parent.DataMemory.document.write("<div class='writeSelected'>");
    else
      parent.DataMemory.document.write("<div class='readSelected'>");
    parent.DataMemory.document.write(unsignedToHexString(arrayIndex * 4, 3) + ":" + this.dataValues[arrayIndex] + "<br>");
    parent.DataMemory.document.write("</div>");

    parent.DataMemory.document.write("<div class ='code'>");
    for (var i = arrayIndex + 1; i < this.dataValues.length; i++) {
      if (this.dataValues[i] != "00000000")
        parent.DataMemory.document.write(unsignedToHexString(i * 4, 3) + ":" + this.dataValues[i] + "<br>");
    }
  }

  parent.DataMemory.document.write("</div></body>");
  parent.DataMemory.document.close();
}

function DataList() {
  this.maxArrayIndex = -1;
  this.dataValues = new Array();
  this.lastLoaded = new Array();

  this.load = load;
  this.reLoad = listReload;
  this.showDataMemory = showDataMemory;
  this.setDataValue = setDataValue;
}

function defineHandlers() {
  var dataArea = document.data.elements[0];
  var loadButton = document.data.elements[1];

  loadButton.onclick = function() {
    dataList.load(dataArea.value)
  };
}
