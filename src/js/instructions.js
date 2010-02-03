// An address as communicated between this and the Java applet is
// a logical address (i.e., a multiple of 4) and it is a
// hex string

   function valueOf(c) {
     if (("0" <= c) && (c <= "9")) return (c-"0");
     switch (c) {
       case "A" : 
	 case "a" : return 10;
       case "B" : 
       case "b" : return 11;
       case "C" : 
	 case "c" : return 12;
       case "D" : 
       case "d" : return 13;
       case "E" : 
       case "e" : return 14;
       case "F" : 
       case "f" : return 15;
     }
   }

   function hexStringToUnsigned(address) {
     var value = 0;
     for (var i=0; i<address.length; i++) 
       value = 16*value + valueOf(address.charAt(i));
     return value;
   }

   function unsignedToHexString(number, numberOfDigits) {
      var result  = "";
	var intermediateValue = number;
	var digitValue;
	var digitCount = 0;
      var temp;
	while (true){
	  digitValue = (intermediateValue % 16);
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

   function isLetter(c) {
     return ( ("a" <= c) && (c <= "z") );
   }

   function isDecimalDigit(c) {
     return ( ("0" <= c) && (c <= "9") );
   }

   function isHexDigit(c) {
     return ( (("0" <= c) && (c <= "9")) ||
              (("A" <= c) && (c <= "F")) );
   }

   function isHexNumber(token, numberOfDigits) {
     token = token.toUpperCase();
     if (token.length == 0) return false;
     for (var i=0; i<token.length; i++)
       if (!isHexDigit(token.charAt(i))) return false;
     if (token.length > numberOfDigits) return false;
     return true;
   }

   function isRegister(token) {
     return ( (token == "$at") || (token == "$v0") || (token == "$v1") || (token == "$a0") || (token == "$a1") ||
              (token == "$a2") || (token == "$a3") || (token == "$t0") || (token == "$t1") || (token == "$t2") ||
              (token == "$t3") || (token == "$t4") || (token == "$t5") || (token == "$t6") || (token == "$t7") ||
              (token == "$s0") || (token == "$s1") || (token == "$s2") || (token == "$s3") || (token == "$s4") ||
              (token == "$s5") || (token == "$s6") || (token == "$s7") || (token == "$t8") || (token == "$t9") ||
              (token == "$k0") || (token == "$s1") || (token == "$gp") || (token == "$sp") || (token == "$fp") ||
              (token == "$ra") || (token == "$zero") );
   }

   function isConstantRegister(token) {
     return (token == "$zero");
   }

   function getRegisterNumber(token) {
     switch (token) {
        case "$at" : return "01";
                     break;
        case "$v0" : return "02";
                     break;
        case "$v1" : return "03";
                     break;
        case "$a0" : return "04";
                     break;
        case "$a1" : return "05";
                     break;
        case "$a2" : return "06";
                     break;
        case "$a3" : return "07";
                     break;
        case "$t0" : return "08";
                     break;
        case "$t1" : return "09";
                     break;
        case "$t2" : return "0A";
                     break;
        case "$t3" : return "0B";
                     break;
        case "$t4" : return "0C";
                     break;
        case "$t5" : return "0D";
                     break;
        case "$t6" : return "0E";
                     break;
        case "$t7" : return "0F";
                     break;
        case "$s0" : return "10";
                     break;
        case "$s1" : return "11";
                     break;
        case "$s2" : return "12";
                     break;
        case "$s3" : return "13";
                     break;
        case "$s4" : return "14";
                     break;
        case "$s5" : return "15";
                     break;
        case "$s6" : return "16";
                     break;
        case "$s7" : return "17";
                     break;
        case "$t8" : return "18";
                     break;
        case "$t9" : return "19";
                     break;
        case "$k0" : return "1A";
                     break;
        case "$k1" : return "1B";
                     break;
        case "$gp" : return "1C";
                     break;
        case "$sp" : return "1D";
                     break;
        case "$fp" : return "1E";
                     break;
        case "$ra" : return "1F";
                     break;
        case "$zero" : return "00";
                       break;
     }
  }

  function fourFieldInstruction(instructionComponents) {
    var temp = hexStringToUnsigned(instructionComponents[0]) & 60;
    var word = unsignedToHexString(Math.floor(temp/4), 1);
    temp =  (hexStringToUnsigned(instructionComponents[0]) &  3)*4;
    temp += Math.floor((hexStringToUnsigned(instructionComponents[1]) & 24)/8);
    word += unsignedToHexString(temp, 1);
    temp =  (hexStringToUnsigned(instructionComponents[1]) & 7)*2;
    temp += Math.floor((hexStringToUnsigned(instructionComponents[2]) & 16)/16);
    word += unsignedToHexString(temp, 1);
    temp =  hexStringToUnsigned(instructionComponents[2]) & 15;
    word += unsignedToHexString(temp, 1);
    temp =  hexStringToUnsigned(instructionComponents[3]);
    word += unsignedToHexString(temp, 4);
    return word;
  }

  function twoFieldInstruction(instructionComponents) {
    var temp = hexStringToUnsigned(instructionComponents[0]) & 60;
    var word = unsignedToHexString(Math.floor(temp/4), 1);
    temp =  (hexStringToUnsigned(instructionComponents[0]) &  3)*4;
    word += unsignedToHexString(temp, 1);
    temp =  hexStringToUnsigned(instructionComponents[1]);
    word += unsignedToHexString(temp, 6);
    return word;
  }

  function sixFieldInstruction(instructionComponents) {
     var temp = hexStringToUnsigned(instructionComponents[0]) & 60;
     var word = unsignedToHexString(Math.floor(temp/4), 1);

     temp =  (hexStringToUnsigned(instructionComponents[0]) &  3)*4;
     temp += Math.floor((hexStringToUnsigned(instructionComponents[1]) & 24)/8);
     word += unsignedToHexString(temp, 1);

     temp =  (hexStringToUnsigned(instructionComponents[1]) & 7)*2;
     temp += Math.floor((hexStringToUnsigned(instructionComponents[2]) & 16)/16);
     word += unsignedToHexString(temp, 1);

     temp =  hexStringToUnsigned(instructionComponents[2]) & 15;
     word += unsignedToHexString(temp, 1);

     temp  = Math.floor((hexStringToUnsigned(instructionComponents[3]) & 30)/2);
     word += unsignedToHexString(temp, 1);

     temp  = (hexStringToUnsigned(instructionComponents[3]) & 1)*8;
     temp += Math.floor((hexStringToUnsigned(instructionComponents[4]) & 28)/4);
     word += unsignedToHexString(temp, 1);

     temp  = (hexStringToUnsigned(instructionComponents[4]) & 3)*4;
     temp += Math.floor((hexStringToUnsigned(instructionComponents[5]) & 48)/16);
     word += unsignedToHexString(temp, 1);

     temp  = hexStringToUnsigned(instructionComponents[5]) & 15;
     word += unsignedToHexString(temp, 1);
     return word;
  }

   function immediateType() {
    try {
       var instructionComponents = new Array();
       var operation = this.tokenList.shift();
       switch (operation) {
          case "addi"  : instructionComponents[0] = "08";
                         break;
          case "andi"  : instructionComponents[0] = "0C";
                         break;
          case "ori"   : instructionComponents[0] = "0D";
                         break;
          case "xori"  : instructionComponents[0] = "0E";
                         break;
        }
        var token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected in first!!");
        if (isConstantRegister(token))
           throw new Error("$zero Register Cannot Be Changed!!");
        instructionComponents[2] = getRegisterNumber(token);

        token = this.tokenList.shift();
        if (token != ",")
           throw new Error("Comma Expected!!");
    
        token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected in second!!");
        instructionComponents[1] = getRegisterNumber(token);
 
        token = this.tokenList.shift();
        if (token != ",")
           throw new Error("Comma Expected!!");

        token = this.tokenList.shift();
        if(!isHexNumber(token, 4))
          throw new Error("Immediate Value of at most 4 Hex Digits Expected!!");
        instructionComponents[3] = token.toUpperCase();
        if (this.tokenList.length != 0)
           throw new Error("Extra text follows instruction!!");

        this.instructions.push(fourFieldInstruction(instructionComponents));
   }
    catch(ex) {
      throw ex;
    }
  }

   function immediateShift() {
    try {
       var instructionComponents = new Array();
       var operation = this.tokenList.shift();
       switch (operation) {
	    case "sll"  : instructionComponents[5] = "00";
                        break;
	    case "sra"  : instructionComponents[5] = "03";
                        break;
	    case "srl"  : instructionComponents[5] = "02";
                        break;
        }
        instructionComponents[0] = "00";
        instructionComponents[1] = "00";
        var token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected!!");
        if (isConstantRegister(token))
           throw new Error("$zero Register Cannot Be Changed!!");
        instructionComponents[3] = getRegisterNumber(token);

        token = this.tokenList.shift();
        if (token != ",")
           throw new Error("Comma Expected!!");
    
        token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected!");
        instructionComponents[2] = getRegisterNumber(token);
 
        token = this.tokenList.shift();
        if (token != ",")
           throw new Error("Comma Expected!!");

        token = this.tokenList.shift();
        if(!isHexNumber(token, 2))
          throw new Error("Immediate Value of at most 2 Hex Digits Expected!!");
        if (hexStringToUnsigned(token) > 31)
          throw new Error("Shift amount must be in the range 0 to 31!!");
        instructionComponents[4] = token.toUpperCase();

        if (this.tokenList.length != 0)
           throw new Error("Extra text follows instruction!!");

        this.instructions.push(sixFieldInstruction(instructionComponents));
   }
    catch(ex) {
      throw ex;
    }
  }

  function variableShift() {
    try {
       var instructionComponents = new Array();
       var operation = this.tokenList.shift();

       switch (operation) {
	    case "sllv" : instructionComponents[5] = "04";
                        break;
	    case "srav" : instructionComponents[5] = "07";
                        break;
	    case "srlv" : instructionComponents[5] = "06";
                        break;
        }
        instructionComponents[0] = "00";
        instructionComponents[4] = "00";

        token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected!!");
        if (isConstantRegister(token))
           throw new Error("$zero Register Cannot Be Changed!!");
        instructionComponents[3] = getRegisterNumber(token);

        token = this.tokenList.shift();
        if (token != ",")
           throw new Error("Comma Expected!!");
    
        token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected!");
        instructionComponents[2] = getRegisterNumber(token);
 
        token = this.tokenList.shift();
        if (token != ",")
           throw new Error("Comma Expected!!");
    
        token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected!");
        instructionComponents[1] = getRegisterNumber(token);
  
       if (this.tokenList.length != 0)
           throw new Error("Extra text follows instruction!!");

        this.instructions.push(sixFieldInstruction(instructionComponents));
     }
     catch (ex) {
        throw ex;
     }
  }

 function dataTransfer() {
    try {
      var instructionComponents = new Array();

      var operation = this.tokenList.shift();
      if (operation == "lw")
         instructionComponents[0] = "23";
      else
         instructionComponents[0] = "2B";
      token = this.tokenList.shift();
      if (!isRegister(token))
         throw new Error("Register Expected!!");
      if ((operation == "lw") && (isConstantRegister(token)))
         throw new Error("$zero Register Cannot Be Changed!!");
      instructionComponents[2] = getRegisterNumber(token);

      token = this.tokenList.shift();
      if (token != ",") 
         throw new Error("Comma Expected!!");

      token = this.tokenList.shift();
      if(!isHexNumber(token))
         throw new Error("Offset Expected!!");
      instructionComponents[3] = token.toUpperCase();

      token = this.tokenList.shift();
      if (token != "(")
         throw new Error("( Expected!!");

      token = this.tokenList.shift();
      if (!isRegister(token))
         throw new Error("Register Expected!!");
      instructionComponents[1] = getRegisterNumber(token);

      token = this.tokenList.shift();
      if (token != ")") 
         throw new Error(") Expected!!");
      if (this.tokenList.length != 0)
         throw new Error("Extra text follows instruction!!");

      this.instructions.push(fourFieldInstruction(instructionComponents));
   }
   catch (ex) {
     throw ex;
   }
  }

  function rType() {
    try {
       var instructionComponents = new Array();
       var operation = this.tokenList.shift();

       switch (operation) {
          case "add"  : instructionComponents[5] = "20";
                        break;
          case "and"  : instructionComponents[5] = "24";
                        break;
          case "or"   : instructionComponents[5] = "25";
                        break;
          case "sub"  : instructionComponents[5] = "22";
                        break;
          case "slt"  : instructionComponents[5] = "2A";
                        break;
	    case "nor"  : instructionComponents[5] = "27";
                        break;
	    case "xor"  : instructionComponents[5] = "26";
                        break;
        }
        instructionComponents[0] = "00";
        instructionComponents[4] = "00";

        token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected!!");
        if (isConstantRegister(token))
           throw new Error("$zero Register Cannot Be Changed!!");
        instructionComponents[3] = getRegisterNumber(token);

        token = this.tokenList.shift();
        if (token != ",")
           throw new Error("Comma Expected!!");
    
        token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected!");
        instructionComponents[1] = getRegisterNumber(token);
 
        token = this.tokenList.shift();
        if (token != ",")
           throw new Error("Comma Expected!!");
    
        token = this.tokenList.shift();
        if (!isRegister(token))
           throw new Error("Register Expected!");
        instructionComponents[2] = getRegisterNumber(token);
  
       if (this.tokenList.length != 0)
           throw new Error("Extra text follows instruction!!");

        this.instructions.push(sixFieldInstruction(instructionComponents));
     }
     catch (ex) {
        throw ex;
     }
  }

  function branch() {
     var instructionComponents = new Array();
     var operation = this.tokenList.shift();

     switch (operation) {
        case "beq" : instructionComponents[0] = "04";
                     break;
        case "bne" : instructionComponents[0] = "05";
                     break;
     }

     var token = this.tokenList.shift();
     if (!isRegister(token))
       throw new Error("Register Expected!!");
     instructionComponents[1] = getRegisterNumber(token);

     token = this.tokenList.shift();
     if (token != ",") 
        throw new Error("Comma Expected!!");

     token = this.tokenList.shift();
     if (!isRegister(token))
        throw new Error("Register Expected!!");
     instructionComponents[2] = getRegisterNumber(token);

     token = this.tokenList.shift();
     if (token != ",") 
        throw new Error("Comma Expected!!");

     token = this.tokenList.shift();
     if(!isHexNumber(token, 4))
        throw new Error("Address Offset of at Most 4 Hex Digits Expected!!");
     instructionComponents[3] = token.toUpperCase();

     if (this.tokenList.length != 0)
        throw new Error("Extra text follows instruction!!");

     this.instructions.push(fourFieldInstruction(instructionComponents));
  }

  function jump() {
    var instructionComponents = new Array();
    instructionComponents[0] = "02";

    this.tokenList.shift();
    token = this.tokenList.shift();
    if (!isHexNumber(token, 7))
       throw new Error("Address of at Most 7 Hex Digits Expected!!");
    instructionComponents[1] = token.toUpperCase();

    this.instructions.push(twoFieldInstruction(instructionComponents));
  }

  function parse() {
     try {
        switch (this.tokenList[0]) {
           case "add"  :                            
           case "and"  : 
           case "or"   : 
           case "sub"  :
           case "slt"  : 
           case "nor"  :
           case "xor"  : this.rType();
                         break;

	     case "sllv" :
           case "srav" :
           case "srlv" : this.variableShift();
                         break;
           case "addi" :
           case "andi" :
           case "ori"  :
           case "xori" : this.immediateType();
                         break;

           case "sll"  :
           case "sra"  :
           case "srl"  : this.immediateShift();
                         break;

           case "lw"   : 
           case "sw"   : this.dataTransfer();
                         break;
           case "beq"  :
           case "bne"  : this.branch();
                         break;
           case "j"    : this.jump();
                         break;
           default     :
             throw new Error("A Valid Assembly Operation Expected!");
        }          
     }
     catch (ex) {
       throw ex;
     }
   }

   function extractTokens(assemblyCode) {
    try {
      var instruction = new Array();
      var token;
      var index;

      while(this.tokenList.length > 0)
         this.tokenList.pop();

      for (var i=0; i<assemblyCode.length; i++)
         instruction.push(assemblyCode.charAt(i));

      while (instruction.length > 0) {
        if (isLetter(instruction[0]) || 
            isDecimalDigit(instruction[0])) {
           token = "";
           while ( (instruction.length > 0) &&
                   (isLetter(instruction[0])  ||
                    isDecimalDigit(instruction[0])) ) {
              token += instruction.shift();
           }
           this.tokenList.push(token);
        }
        else if (instruction[0] == "$") {
          if (isLetter(instruction[1])) {
             token = "";
             token += instruction.shift();
             while ( (isLetter(instruction[0]) || isDecimalDigit(instruction[0])) &&
                     (instruction.length > 0) ) {               
                token += instruction.shift();
             }
             this.tokenList.push(token);
          }
          else 
             throw new Error("A Letter is Expected!");
       }
       else {
         token = "";
         switch (instruction[0]) {
            case "(" : token += instruction.shift();
                       this.tokenList.push(token);
                       break;
            case ")" : token += instruction.shift();
                       this.tokenList.push(token);
                       break;
            case "," : token += instruction.shift();
                       this.tokenList.push(token);
                       break;
            default : 
                       throw new Error("Illegal character " + instruction[0]);
         }
       }
       while ( (instruction.length > 0) && 
               ((instruction[0].charCodeAt(0) < 33) || (instruction[0].charCodeAt(0) > 176)) ) {
         instruction.shift();
       }
     }
    }
    catch (ex) {
      throw ex;
    }
   }


   function assembleLoad() {
     try {
        for (var i=0; i < this.temp2.length; i++) {
          var assemblyCode  = this.temp2[i];
          var index = assemblyCode.indexOf("#");
          if (index >= 1)
             assemblyCode = assemblyCode.substr(0, index);
          assemblyCode = assemblyCode.toLowerCase();
          this.extractTokens(assemblyCode); 

          this.parse();
       }

      for (var i=0; i < this.instructions.length; i++) {
        var address = unsignedToHexString(i*4);
	  top.DataPath.setInstruction(this.instructions[i], address);
        this.lastLoaded[i] = this.instructions[i];
      }
  
      top.DataPath.resetDataPath();

      this.showInstructions("");

       alert("Instructions Successfully Assembled and Loaded into Memory!");
     }
     catch (ex) {
        alert("ERROR\n" +
              ex.message + "\nin\n" +
              assemblyCode +
              "\nAssembly Terminated!");
     }
   }

   function resetMachine() {
      top.DataPath.initInstructions();
      top.DataPath.resetDataPath();
      top.DataInput.reLoad();
      top.RegisterInput.reLoad();

      while (this.instructions.length > 0) 
        this.instructions.pop();

      for (var i=0; i<this.lastLoaded.length; i++) {
         var address = unsignedToHexString(i*4, 3);
         top.DataPath.setInstruction(this.lastLoaded[i], address);
         this.instructions[i] = this.lastLoaded[i];
      }
      this.showInstructions("");
   }
          
   function load(code) { 	
      var temp = new Array();
      var index = 0;

      if (code.length == 0) return;

      if (top.getBrowserName() == "Microsoft Internet Explorer") 
         temp = code.split(/\n/);		// For IE: split on newline
      else {                           // For Netscape: replace \n with space\n and split on \n
         var codeString = "";
         for(i=0; i<code.length; i++)
           if (code.charAt(i) == "\n")
              codeString = codeString + " \n";
           else
              codeString = codeString + code.charAt(i)
         temp = codeString.split(/\n/);		
      }

      for (var i=0; i<temp.length; i++)
        temp[i] = temp[i].substr(0, temp[i].length-1);

      while (this.temp2.length > 0)
         this.temp2.pop();

	for (var i=0; i<temp.length; i++) {
         index = 0;
         while ((index < temp[i].length) && (temp[i].charAt(index) == " "))
            index++;
         temp[i] = temp[i].substr(index);
         if ((temp[i].charAt(0) != '#') && (temp[i].length > 0))
             this.temp2.push(temp[i].toUpperCase());
      }
      while(this.instructions.length > 0)
         this.instructions.pop();
      while(this.lastLoaded.length > 0)
         this.lastLoaded.pop();
      top.DataPath.initInstructions;

      this.assembleLoad();
  }

  function returnedInstructionAccess(address) {
     var addressString = new String(address);
     if (addressString.length > 0)
        codeList.showInstructions(addressString);
  }

  function showInstructions(address) {
    var memoryIndex;
    top.InstructionMemory.document.open();
    top.InstructionMemory.document.write("<head>");
    top.InstructionMemory.document.write("<LINK HREF='pathsim.css' REL='stylesheet' type='text/css'>");
    top.InstructionMemory.document.write("</head><body>");
    top.InstructionMemory.document.write("<div class='boxHead'>Instruction Memory</div>");

    top.InstructionMemory.document.write("<div class ='code'>");
    if (address.length == 0) {
       for (var i=0; i<this.instructions.length; i++) {
         if (this.instructions[i] != "00000000")
           top.InstructionMemory.document.write(unsignedToHexString(i*4, 3) + ":" + this.instructions[i] + "<br>");
       }
    }
    else {
	 memoryIndex = hexStringToUnsigned(address)/4;
       for (var i=0; i<memoryIndex; i++) {
         if (this.instructions[i] != "00000000")
           top.InstructionMemory.document.write(unsignedToHexString(i*4, 3) + ":" + this.instructions[i] + "<br>");
       }
       top.InstructionMemory.document.write("</div>");

	 top.InstructionMemory.document.write("<div class ='readSelected'>");
       top.InstructionMemory.document.write(unsignedToHexString(memoryIndex*4, 3) + ":" + this.instructions[memoryIndex] + "<br>");
       top.InstructionMemory.document.write("</div>");

       top.InstructionMemory.document.write("<div class ='code'>");
       for (var i=memoryIndex+1; i<this.instructions.length; i++) {
         if (this.instructions[i] != "00000000")
           top.InstructionMemory.document.write(unsignedToHexString(i*4, 3) + ":" + this.instructions[i] + "<br>");
       }
    }
    top.InstructionMemory.document.write("</div></body>");
    top.InstructionMemory.document.close();
  }

  function defineHandlers()  { 
    var codeArea      = document.instructions.elements[0];
    var loadButton    = document.instructions.elements[1];
    var resetButton   = document.instructions.elements[2];

    loadButton.onclick    = function() {codeList.load(codeArea.value)};
    resetButton.onclick   = function() {codeList.resetMachine()};
  }

  function CodeList() {
    this.instructions = new Array();
    this.lastLoaded   = new Array();
    this.temp2        = new Array();
    this.tokenList    = new Array();

    this.resetMachine     = resetMachine;
    this.load             = load;
    this.assembleLoad     = assembleLoad;
    this.extractTokens    = extractTokens;
    this.parse            = parse;
    this.immediateType    = immediateType;
    this.immediateShift   = immediateShift;
    this.variableShift    = variableShift;
    this.dataTransfer     = dataTransfer;
    this.rType            = rType;
    this.branch           = branch;
    this.jump             = jump;
    this.showInstructions = showInstructions;
  }
