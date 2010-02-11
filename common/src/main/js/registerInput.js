// Communication of a register index from this script to the Java applet
// is as a decimal integer BUT coming back, it is a decimal string
// since calls to the Java Script from the applet can only pass 
// String parameters

   function isRegisterNumber(registerNumber) {
     for (var i=0; i<registerNumber.length; i++)
       if ((registerNumber.charAt(i) < "0") ||
           (registerNumber.charAt(i) > "9")) return false;
     var value = decimalStringToUnsigned(new String(registerNumber));
     if ((value < 0) || (value > 31)) return false;
     return true;
   }

   function valueOf(c) {
     if (("0" <= c) && (c <= "9")) return (c-"0");
   }
          
   function decimalStringToUnsigned(registerNumber) {
     var value = 0;
     for (var i=0; i<registerNumber.length; i++) 
       value = 10*value + valueOf(registerNumber.charAt(i));
     return value;
   }

   function unsignedToDecimalString(number, numberOfDigits) {
      var result  = "";
	var intermediateValue = number;
	var digitValue;
	var digitCount = 0;
      var temp;
	while (true){
	  digitValue = (intermediateValue % 10);
	  switch (digitValue){
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
	  }

	  digitCount++;
	  temp = "" + (intermediateValue /= 10); 
        if (temp.indexOf(".") == 0)
           intermediateValue = 0;
        else if (temp.indexOf(".") == -1) {
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

   function load(registerValues)  {
 	var temp  = new Array();
	var temp2 = new Array();
      var index = 0;
	var word;
	var registerNumber;
	var registerNumberValue;

      if (registerValues.length == 0) return;

      while (temp.length > 0) 
        temp.pop();
      while (temp2.length > 0)
         temp2.pop();

      if (top.getBrowserName() == "Microsoft Internet Explorer") 
         temp = registerValues.split(/\n/);		// For IE: split on newline
      else {						// For Netscape: replace \n with space\n and split on \n
         var valueString = "";
         for(i=0; i<registerValues.length; i++)
           if (registerValues.charAt(i) == "\n")
              valueString = valueString + " \n";
           else
              valueString = valueString + registerValues.charAt(i)
         temp = valueString.split(/\n/);		
      }

	for (var i=0; i<temp.length; i++) {
         temp[i] = temp[i].substr(0, temp[i].length-1);
         var line = "";
         var index = 0;
         while ((index < temp[i].length) && (temp[i].charAt(index) == " "))
            index++
         if (temp[i].substr(index).length > 0)
             temp2.push(temp[i].substr(index));
      }

      for (var i=0; i<temp2.length; i++)
        temp2[i] = temp2[i].toUpperCase();

	for (var i=0; i<temp2.length; i++) {
         index = temp2[i].indexOf(":");
         if ((index == -1) || (index == 0)) {
            alert("ERROR: Register value " + temp2[i] + 
                  "\nMust be given with a register number" +
                  "\nRegister Loading Terminated!");
            return;
         }

         registerNumber = temp2[i].substr(0, index);
         if (!isRegisterNumber(registerNumber)) {
           alert("ERROR: Register Number " + registerNumber + 
                 "\nmust be an decimal number between 1 and 31.\nRegister Loading Terminated!");
           return;   
         }
         else if (decimalStringToUnsigned(new String(registerNumber)) == 0) {
            alert("ERROR: Register 0 is Constant Holding 00000000" +
                "\n       Its Value CANNOT be Changed!" +
                "\nRegister Loading Terminated!");
            return;   
         }
      }

      for (var i=0; i < temp2.length; i++) {
        index = temp2[i].indexOf(":");
        word = temp2[i].substr(index+1);
        if (word.length != 8) {
          alert("ERROR: " + word +
                "\nMust have 8 hex digits!!\nRegister Loading Terminated!");
          return;
        }
        for (var j=0; j<word.length; j++) {
          var digit = word.charAt(j);
          if (((digit < "0") || (digit > "9")) &&
              ((digit < "A") || (digit > "F"))) {
              alert("ERROR: " + word + 
                    "\nHas an ILLEGAL character (not a Hex digit)!!\nRegister Loading Terminated!");
              return;
          }
        }
      } 

      this.zeroRegisters();

      for (var i=0; i < temp2.length; i++) {
        index = temp2[i].indexOf(":");
        var register      = temp2[i].substr(0, index);
        var registerIndex = decimalStringToUnsigned(new String(register));
        if (registerIndex != 0) {
           word = temp2[i].substr(index+1);
	     top.DataPath.setRegister(word, registerIndex);
           this.registerValues[registerIndex] = word;
           this.lastLoaded[registerIndex] = word;
        }
      }

      this.showRegisters00();
      alert("Registers Successfully Loaded!");
   }

  function zeroRegisters() {
    for (var i=0; i<this.numberOfRegisters; i++) {
      this.registerValues[i] = "00000000";
      this.lastLoaded[i]     = "00000000";
    }
    top.DataPath.zeroRegisters();
  }

  function setRegister(registerIndex, value) {
     this.registerValues[registerIndex] = value;
  }

  function reLoad() {
     registerList.reLoad();
  }

  function listReload() {
     for (var i=0; i<this.numberOfRegisters; i++) {
        this.registerValues[i] = this.lastLoaded[i];
        if (this.lastLoaded[i] != 0)
           top.DataPath.setRegister(this.lastLoaded[i], i);
     }
     this.showRegisters00();
  }

  function returnedRegisterAccess(read1, read2, write, value) {
     var read1Index = decimalStringToUnsigned(new String(read1));
     var read2Index = decimalStringToUnsigned(new String(read2));
     var writeString = new String(write);
     if (writeString.length > 0) {
        var writeIndex = decimalStringToUnsigned(writeString);
        registerList.setRegister(writeIndex, value);
        if (read1Index == read2Index) {
           if (writeIndex == read1Index) {
              registerList.showRegisters01(writeIndex);
           }
           else {
              registerList.showRegisters11(read1Index, writeIndex);        
           }
        }
        else if (writeIndex == read1Index) {
           registerList.showRegisters11(read2Index, writeIndex);
        }
        else if (writeIndex == read2Index) {
           registerList.showRegisters11(read1Index, writeIndex);
        }
        else {
           registerList.showRegisters21(read1Index, read2Index, writeIndex);
        }
     }
     else {
        if (read1Index == read2Index) {
           registerList.showRegisters10(read1Index);
        }
        else if (read1Index < read2Index) {
           registerList.showRegisters20(read1Index, read2Index);
        }
        else {
           registerList.showRegisters20(read2Index, read1Index);
        }
     }
  }

  function writeStyles() {
    top.RegisterDisplay.document.open();
    top.RegisterDisplay.document.write("<head>");
    top.RegisterDisplay.document.write("<LINK HREF='pathsim.css' REL='stylesheet' type='text/css'>");
    top.RegisterDisplay.document.write("</head><body>");
    top.RegisterDisplay.document.write("<div class='boxHead'>Registers</div>");
  }
  
  function numberOrder(a, b) {return a-b;}

  function showRegisters21(read1Index, read2Index, writeIndex) {
    writeStyles();

    var indices = new Array(3);
    indices[0]  = read1Index;
    indices[1]  = read2Index;
    indices[2]  = writeIndex;
    indices.sort(numberOrder);
    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=0; i<indices[0]; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div>");

    if (indices[0] == writeIndex)
       top.RegisterDisplay.document.write("<div class='writeSelected'>");
    else
       top.RegisterDisplay.document.write("<div class='readSelected'>");
    top.RegisterDisplay.document.write(unsignedToDecimalString(indices[0], 2) + ":" + this.registerValues[indices[0]] + "<br>");
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=indices[0]+1; i<indices[1]; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div>");

    if (indices[1] == writeIndex)
       top.RegisterDisplay.document.write("<div class='writeSelected'>");
    else
       top.RegisterDisplay.document.write("<div class='readSelected'>");
    top.RegisterDisplay.document.write(unsignedToDecimalString(indices[1], 2) + ":" + this.registerValues[indices[1]] + "<br>");
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=indices[1]+1; i<indices[2]; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div>");

     if (indices[2] == writeIndex)
       top.RegisterDisplay.document.write("<div class='writeSelected'>");
    else
       top.RegisterDisplay.document.write("<div class='readSelected'>");
    top.RegisterDisplay.document.write(unsignedToDecimalString(indices[2], 2) + ":" + this.registerValues[indices[2]] + "<br>");
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=indices[2]+1; i<this.numberOfRegisters; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div></body");
    top.RegisterDisplay.document.close();
  }

  function showRegisters11(readIndex, writeIndex) {
    writeStyles();

    var indices = new Array(2);
    indices[0] = readIndex;
    indices[1] = writeIndex;
    indices.sort(numberOrder);

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=0; i<indices[0]; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div>");

    if (indices[0] == writeIndex)
       top.RegisterDisplay.document.write("<div class='writeSelected'>");
    else
       top.RegisterDisplay.document.write("<div class='readSelected'>");
    top.RegisterDisplay.document.write(unsignedToDecimalString(indices[0], 2) + ":" + this.registerValues[indices[0]] + "<br>");
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=indices[0]+1; i<indices[1]; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div>");

    if (indices[1] == writeIndex)
       top.RegisterDisplay.document.write("<div class='writeSelected'>");
    else
       top.RegisterDisplay.document.write("<div class='readSelected'>");
    top.RegisterDisplay.document.write(unsignedToDecimalString(indices[1], 2) + ":" + this.registerValues[indices[1]] + "<br>");
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=indices[1]+1; i<this.numberOfRegisters; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div></body>");
    top.RegisterDisplay.document.close();
  }

  function showRegisters01(writeIndex) {
    writeStyles();

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=0; i<writeIndex; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='writeSelected'>");
    top.RegisterDisplay.document.write(unsignedToDecimalString(writeIndex, 2) + ":" + this.registerValues[writeIndex] + "<br>");
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=writeIndex+1; i<this.numberOfRegisters; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div></body>");
    top.RegisterDisplay.document.close();
  }

  function showRegisters20(smallerIndex, largerIndex) {
    writeStyles();

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=0; i<smallerIndex; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='readSelected'>");
    top.RegisterDisplay.document.write(unsignedToDecimalString(smallerIndex, 2) + ":" + this.registerValues[smallerIndex] + "<br>");
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=smallerIndex+1; i<largerIndex; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='readSelected'>");
    top.RegisterDisplay.document.write(unsignedToDecimalString(largerIndex, 2) + ":" + this.registerValues[largerIndex] + "<br>");
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=largerIndex+1; i<this.numberOfRegisters; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div></body>");
    top.RegisterDisplay.document.close();
  } 

  function showRegisters10(readIndex) {
    writeStyles();

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=0; i<readIndex; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='readSelected'>");
    top.RegisterDisplay.document.write(unsignedToDecimalString(readIndex, 2) + ":" + this.registerValues[readIndex] + "<br>");
    top.RegisterDisplay.document.write("</div>");

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=readIndex+1; i<this.numberOfRegisters; i++) {
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");
    }
    top.RegisterDisplay.document.write("</div></body>");
    top.RegisterDisplay.document.close();
  }

  function showRegisters00() {
    writeStyles();

    top.RegisterDisplay.document.write("<div class ='code'>");
    for (var i=0; i<this.numberOfRegisters; i++)
       top.RegisterDisplay.document.write(unsignedToDecimalString(i, 2) + ":" + this.registerValues[i] + "<br>");

    top.RegisterDisplay.document.write("</div></body>");
    top.RegisterDisplay.document.close();
  }

  function defineHandlers()  {
    var registerArea  = document.registers.elements[0];
    var loadButton    = document.registers.elements[1];
    var zeroButton    = document.registers.elements[2];

    loadButton.onclick  = function() {registerList.load(registerArea.value);}
    zeroButton.onclick  = function() {registerList.zeroRegisters();  document.registers.elements[0].value = "";
						  registerList.showRegisters00();}
  }

  function RegisterList() {
    this.numberOfRegisters = 32;
    this.registerValues    = new Array(this.numberOfRegisters);
    this.lastLoaded        = new Array(this.numberOfRegisters);

    this.load              = load;
    this.reLoad            = listReload;
    this.zeroRegisters     = zeroRegisters;
    this.showRegisters00   = showRegisters00;
    this.showRegisters10   = showRegisters10;
    this.showRegisters20   = showRegisters20;
    this.showRegisters01   = showRegisters01;
    this.showRegisters11   = showRegisters11;
    this.showRegisters21   = showRegisters21;
    this.setRegister       = setRegister;
  }
