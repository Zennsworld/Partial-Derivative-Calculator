#include <iostream>
#include <ctype.h>
#include <cmath>

using namespace std;

const short PLUS = 0;
const short MINUS = 1;
const short TIMES = 2;
const short DIVIDE = 3;
const short POWER = 4;
const short CHAIN = 5;

const string functionArray[2][7] = {{"log","sin","cos","tan","csc","sec","cot"},
																		{"1/(~)","cos(~)","-sin(~)","sec(~)^2","-csc(~)*cot(~)","sec(~)*tan(~)","-csc(~)^2"}};

struct Constant;
struct Strings;
struct Command;
struct CommandOperator;
struct CommandFunction;
struct CommandFundamentalFunction;
struct Level;
struct IndexOperator;
struct Algebra;
struct AlgebraList;
struct AlgebraOperator;
struct AlgebraFunction;

short isOperator (char op);
string lowestOperator (string function);
int isFundamentalFunction (string function);
Command* buildFunctionTree (string constants [], int startIL, Level* currentLevel, string function, string variable);
Constant* fixInput (string function, string variable, int &constantNum);
Level* breakInput (string function, int* i);
Algebra* buildFunctionGraph (Level* start, int startIL, string function, string variable);
Algebra* redirect (Algebra* start);
Algebra* condenseSwitchConnected (Algebra* f, Algebra* g, string op, bool inNumber);
Algebra* condenseSwitchOperator (AlgebraList** connectedBy);
Algebra* condenseSwitchUnConnected (Algebra* f, Algebra* g, string opOn);
string condenseSwitchFundamental (int function, string value, AlgebraList** g);
void deleteGraphNode (Algebra* node);
string simplify (string function, string variable);
Strings PLUSRULE (Strings f, Strings g);
Strings MINUSRULE (Strings f, Strings g);
Strings PRODUCTRULE (Strings f, Strings g);
Strings QUOTIENTRULE (Strings f, Strings g);
Strings POWERRULE (Strings f, Strings g);

//A linked list that stores the constants
struct Constant {
	string constant;
	Constant* next;
};

//Simply a struct to hold two string values
struct Strings {
	string derivative;
	string original;
	Strings (){
		derivative = "";
		original = "";
	}
};

//The base struct that is used by extending into other command structs (below) that are used for the function tree
struct Command {
	Strings (* diffRule)(Strings f, Strings g);
	virtual Strings getFunctions () = 0;
	bool negative;
};

//The command operator is for operator nodes in the tree; they use the function pointer to return the correct differentiation rule
struct CommandOperator : Command {
	Command* f;
	Command* g;
	Strings getFunctions (){
		Strings temp = diffRule (f->getFunctions(), g->getFunctions());
		if (negative){
			temp.derivative = "-"+temp.derivative;
			temp.original = "-"+temp.original;
		}
		return temp;
	}
};

//The command function stores constants and the variable and returns the respective derivative
struct CommandFunction : Command {
	string function;
	string derivative;
	Strings getFunctions (){
		Strings temp;
		temp.original = "";
		temp.derivative = "";
		if (negative){
			temp.original += "-";
		}
		temp.original += function;
		if (negative){
			temp.derivative += "-";
		}
		temp.derivative += derivative;
		return temp;
	}
};

//The command fundamental function stores log and trig functions
//They use the lookup table above to get the derivative, replacing the ~ with whatever is inside the function
struct CommandFundamentalFunction : Command {
	int function;
	Command* inside;
	Strings getFunctions (){
		Strings temp;
		Strings insidetemp = inside->getFunctions();
		temp.original = "";
		temp.derivative = "";
		if (negative){
			temp.original += "-";
		}
		temp.original += functionArray[0][function]+"("+insidetemp.original+")";
		string dertemp = functionArray[1][function];
		if (negative){
			temp.derivative += "-";
		}
		for (int i=0;i<dertemp.length();i++){
			if (dertemp[i]=='~'){
				temp.derivative += insidetemp.original;
			} else {
				temp.derivative += dertemp[i];
			}
		}
		temp.derivative = "("+temp.derivative+")*"+insidetemp.derivative;
		return temp;
	}
};

//The struct that is used for parenthesis handling, it is a tree where each node has any nuber of children and it is traversed a little bit differently
struct Level {
	string function;
	Level* nextLevel;
	Level* next;
	Level (){
		function = "";
		nextLevel = NULL;
		next = NULL;
	}
};

//A linked list to keep track of operators during the graph making
struct IndexOperator {
	int index;
	short operatorID;
	int split;
	int splitAfter;
	IndexOperator* next;
};

//A base struct for the graph that is used by extending
struct Algebra {
	string value;
	struct AlgebraList* connectedNodes;
	virtual void condense() = 0;
	virtual string getFunction () = 0;
};

//A linked list of algebra structs
struct AlgebraList {
	struct Algebra* algebra;
	AlgebraList* next;
};

//The algebra function holds a function in a node for the graph
struct AlgebraFunction : Algebra {
	AlgebraFunction (string value){
		this->value = value;
		connectedNodes = NULL;
	}
	void condense (){

	}
	string getFunction (){
		int index = value.find('*', 0);
		if (index>-1){
			if (index==1&&value[0]=='1'){
				value = value.substr(2);
			} else if (index==1&&value[0]=='0'){
				return "0";
			}
		}
		index = value.find('^', 0);
		if (index>-1){
			if (index==1&&value[2]=='1'){
				value = value.substr(0,1);
			} else if (index==1&&value[2]=='0'){
				return "1";
			}
		}
		if (value.find('.')!=string::npos){
			int i = value.length()-1;
			for (;i>-1;i--){
				if (value[i]!='0'){
					break;
				}
			}
			if (i>-1){
				value.erase(i+1, string::npos);
			}
		}
		if (value[value.length()-1]=='.'){
			value = value.substr(0, value.length()-1);
		}
		return value;
	}
};

//The algebra fundamental function holds log and trig functions in a node for the graph
struct AlgebraFundamentalFunction : Algebra {
	int function;
	AlgebraFundamentalFunction (string value){
		function = stoi(value.substr(1,1));
		this->value = value.substr(0,1);
		connectedNodes = NULL;
	}
	void condense (){
		this->value = condenseSwitchFundamental (function, this->value, &connectedNodes);
	}
	string getFunction (){
		return functionArray[0][function]+"("+connectedNodes->algebra->getFunction()+")";
	}
};

//The algebra operator holds an operator for the graph and connects everything else to it
struct AlgebraOperator : Algebra {
	AlgebraOperator (string value){
		this->value = value;
		connectedNodes = NULL;
	}
	void condense (){
		AlgebraList* trav1 = connectedNodes;
		AlgebraList* trav2 = connectedNodes;
		while (trav1!=NULL){
			//it turns from a fund. function to normal funciton and gets confused
			if (trav1->algebra->value=="_"){
				trav1->algebra->condense();
				Algebra* temp = trav1->algebra;
				AlgebraFunction* af = new AlgebraFunction(trav1->algebra->value);
				trav1->algebra = af;
				delete temp;
			} else {
				trav1->algebra->condense();
			}
			if (isOperator(trav1->algebra->value[0])>-1) {
				condenseSwitchOperator(&trav1);
				if (trav1==NULL){
					break;
				}
			}
			trav2 = trav1->algebra->connectedNodes;
			if (trav2!=NULL){
				if (isdigit(trav1->algebra->value[0])){
					AlgebraList* temp = trav2;
					trav1->algebra = condenseSwitchConnected(trav1->algebra, trav2->algebra, value, true);
					trav1->algebra->connectedNodes = trav2->algebra->connectedNodes;
					delete temp->algebra;
					delete temp;
				} else if (isOperator(trav1->algebra->value[0])==-1) {
					AlgebraList* temp = trav2;
					trav1->algebra = condenseSwitchConnected(trav1->algebra, trav2->algebra, value, false);
					trav1->algebra->connectedNodes = trav2->algebra->connectedNodes;
					delete temp->algebra;
					delete temp;
				} else {
					trav1 = trav1->next;
				}
			} else {
				trav1 = trav1->next;
			}
		}
	}
	string getFunction (){
		AlgebraList* trav = connectedNodes;
		string function = "";
		string temp = "";
		while (trav!=NULL){
			temp = trav->algebra->getFunction();
			function += "("+temp+")";
			trav = trav->next;
			if (trav!=NULL){
				function += value;
			}
		}
		if (function==""){
			function = "Wut";
		}
		return function;
	}
};

//Function pointer array for faster access
Strings (* const RULES [])(Strings f, Strings g) = {PLUSRULE,MINUSRULE,PRODUCTRULE,QUOTIENTRULE,POWERRULE};

//This method runs everything in serial; there is little hidden here
int main (){
	cout<<"Enter Function"<<endl;
	string function;
	cin>>function;
	cout<<endl<<"Enter Variable"<<endl;
	string variable;
	cin>>variable;
	int constantSize = 0;
	Constant* constants = fixInput(function, variable, constantSize);
	function = constants->constant;
	Constant* temp = constants->next;
	delete constants;
	constants = temp;
	string constantArray [constantSize];
	for (int i=0;i<constantSize;i++){
		constantArray[i] = constants->constant;
		Constant* temp = constants->next;
		delete constants;
		constants=temp;
	}
	cout<<endl<<"Function Going In:"<<endl<<function<<endl;
	int* index = new int(0);
	Level* start = breakInput (function, index);
	delete index;
	cout<<endl<<"Command Tree: "<<endl;
	Command* root = buildFunctionTree(constantArray, 0, start, start->function, variable);
	cout<<endl<<"Function Coming Out:"<<endl;
	cout<<(*root).getFunctions().original<<endl;
	cout<<endl<<"Raw Derivative:"<<endl;
	string rawDerivative = (*root).getFunctions().derivative;
	cout<<rawDerivative<<endl;
	cout<<endl<<"Simplification Graph Process:"<<endl;
	string finalDerivative = simplify(rawDerivative, variable);
	cout<<endl<<"Final Derivative:"<<endl;
	cout<<finalDerivative<<endl;
}

//f'(x) + g'(x)
Strings PLUSRULE (Strings f, Strings g){
	Strings temp;
	temp.derivative = "("+f.derivative+"+"+g.derivative+")";
	temp.original = "("+f.original+"+"+g.original+")";
	return temp;
}

//f'(x) - g'(x)
Strings MINUSRULE (Strings f, Strings g){
	Strings temp;
	temp.derivative = "("+f.derivative+"-"+g.derivative+")";
	temp.original = "("+f.original+"-"+g.original+")";
	return temp;
}

//f(x)g'(x)+f'(x)g(x)
Strings PRODUCTRULE (Strings f, Strings g){
	Strings temp;
	temp.derivative = "("+f.original+"*"+g.derivative+"+"+f.derivative+"*"+g.original+")";
	temp.original = "("+f.original+"*"+g.original+")";
	return temp;
}

//(g(x)f'(x)-g'(x)f(x))/g^2(x)
Strings QUOTIENTRULE (Strings f, Strings g){
	Strings temp;
	temp.derivative = "(("+g.original+"*"+f.derivative+"-"+g.derivative+"*"+f.original+")/("+g.original+")^2)";
	temp.original = "("+f.original+"/"+g.original+")";
	return temp;
}

//d/dx f(x)^g(x) -> too messy for being useful here
Strings POWERRULE (Strings f, Strings g){
	Strings temp;
	temp.derivative = "(("+f.original+"^"+g.original+")*(("+f.derivative+"*"+g.original+")/("+f.original+")+("+g.derivative+"*log("+f.original+"))))";
	temp.original = "("+f.original+"^"+g.original+")";
	return temp;
}

//Checks if a character is +,-,*,/,_ and returns the consts above if any (otherwise -1)
short isOperator (char op){
	switch (op){
		case '+':
		return PLUS;
		case '-':
		return MINUS;
		case '*':
		return TIMES;
		case '/':
		return DIVIDE;
		case '^':
		return POWER;
		case '_':
		return CHAIN;
		default:
		return -1;
	}
}

//Checks to see if there is an operator and if so returns the lowest one and its index
string lowestOperator (string function){
	short smallest = 9;
	int smallestIndex = -1;
	for (int i=0;i<function.length();i++){
		//- or negative needs to be handled
		short temp = isOperator(function[i]);
		if (temp==PLUS||temp==MINUS){
			smallest = temp;
			smallestIndex = i;
		} else if (smallest>1&&(temp==TIMES||temp==DIVIDE)){
			smallest = temp;
			smallestIndex = i;
		} else if (smallest>3&&(temp==POWER)){
			smallest = temp;
			smallestIndex = i;
		} else if (smallest>5&&temp==CHAIN){
			smallest = temp;
			smallestIndex = i;
		}
	}
	return to_string(smallest) + to_string(smallestIndex);
}

//Checks to see if a 3 character sequence is a fundamental function or not using the lookup table above. It will return the index of the table or -1
int isFundamentalFunction (string function){
	for (int i=0;i<7;i++){
		if (function==functionArray[0][i]){
			return i;
		}
	}
	return -1;
}

//This function builds the entire function tree that is used for differentiation
//It uses order of operators in order to break up the function so that the differentiation rules are done correctly
Each node has 2 or 0 children
//The tree is a command struct tree, though there are no raw command instances (that wouldn't make sense)
Command* buildFunctionTree (string constants [], int startIL, Level* currentLevel, string function, string variable){
	static int constantIndex = 0;
	string lowest = lowestOperator(function);
	short smallest = (short)(stoi (lowest.substr(0,1)));
	int smallestIndex = stoi (lowest.substr(1));
	if (smallestIndex>0){
		string f = function.substr(0, smallestIndex);
		string g = function.substr(smallestIndex+1);
		int atSplit = startIL;
		for (int i=0;i<f.length();i++){
			if (f[i]=='@'){
				atSplit++;
			}
		}
		cout<<f<<"   "<<smallest<<"   "<<g<<endl;
		CommandOperator* command = new CommandOperator;
		command->diffRule = RULES[smallest];
		if (function[0]=='$'){
			command->negative = true;
			f = f.substr(1);
		} else if (function[0]=='_'&&function[1]=='$'){
			command->negative = true;
			f = "_" + f.substr(2);
		}	else {
			command->negative = false;
		}
		command->f = buildFunctionTree(constants, startIL, currentLevel, f, variable);
		command->g = buildFunctionTree(constants, atSplit, currentLevel, g, variable);
		return command;
	} else {
		if (function=="@"){
			currentLevel=currentLevel->nextLevel;
			Level* temp = currentLevel;
			for (int i=0;i<startIL;i++){
				temp = temp->next;
			}
			return buildFunctionTree(constants, 0, temp, temp->function, variable);
		} else if (function=="?"||function==(variable+"$")||function==variable){
			CommandFunction* command = new CommandFunction;
			if (function=="?"){
				command->function = constants[constantIndex];
				if (command->function[command->function.length()-1]=='$'||command->function[0]=='$'){
					command->negative = true;
					command->function = command->function.substr(0, command->function.length()-1);
				} else {
					command->negative = false;
				}
				command->derivative = "0";
				constantIndex++;
			} else {
				command->function = function;
				if (command->function[command->function.length()-1]=='$'){
					command->negative = true;
					command->function = command->function.substr(0, command->function.length()-1);
				} else {
					command->negative = false;
				}
				command->derivative = "1";
			}
			return command;
		} else if (function[0]=='_'){
			CommandFundamentalFunction* command = new CommandFundamentalFunction;
			command->function = stoi(string (1, function[1]));
			command->inside = buildFunctionTree(constants, startIL, currentLevel, "@", variable);
			if (function[2]=='$'){
				command->negative = true;
			} else {
				command->negative = false;
			}
			return command;
		}
	}
	cout<<"WAIT"<<endl;
	return NULL;
}

//This function parses the input of the user
//It turns constants (numbers and letter) into "?" in the string and stores them into a constant linked list
//It turns fundamental functions into _ with an index of the lookup table: i.e logx = _0(x)
//It turns negatives into $ in specific spaces
//It adds in implied multiplications
Constant* fixInput (string function, string variable, int &constantNum){
	string newFunction = "";
	Constant* head = new Constant();
	Constant* current = head;
	bool sameNumber = false;
	bool addDollar = false;
	for (int i=0;i<function.length();i++){
		if (isdigit(function[i])||function[i]=='.'){
			if (sameNumber){
				current->constant += function[i];
			} else {
				constantNum++;
				newFunction += "?";
				sameNumber = true;
				current->next = new Constant();
				current = current->next;
				if (addDollar){
					current->constant += "$";
					addDollar = false;
				}
				current->constant += function[i];
			}
		} else if (isOperator(function[i])==-1){
				sameNumber = false;
				if (function[i]!=variable.at(0)&&function[i]!='('&&function[i]!=')'){
					int functionCheck = isFundamentalFunction(function.substr(i, 3));
					if (functionCheck>-1){
						newFunction += "_";
						newFunction += to_string(functionCheck);
						if (addDollar){
							newFunction += "$";
							addDollar = false;
						}
						i=i+2;
					} else {
						constantNum++;
						newFunction+="?";
						current->next = new Constant();
						current = current->next;
						current->constant = string (1, function[i]);
						if (addDollar){
							current->constant += "$";
							addDollar = false;
						}
					}
				} else {
					newFunction += function[i];
					if ((function[i]==variable.at(0)&&addDollar)||(function[i]=='('&&addDollar)){
						newFunction += "$";
						addDollar = false;
					}
				}
		} else if ((i>0&&function[i]=='-'&&isOperator(function[i-1])>-1)||(i>0&&function[i]=='-'&&function[i-1]=='(')||(i==0&&function[0]=='-')){
			addDollar = true;
		} else {
			addDollar = false;
			sameNumber = false;
			newFunction += function[i];
		}
	}
	function = newFunction;
	newFunction = "";
	for (int i=0;i<function.length();i++){
		newFunction += function[i];
		if (function[i]=='_'){
			newFunction += function[i+1];
			int checkvar = i+2;
			if (function[checkvar]=='$'){
				newFunction+=function[checkvar];
				checkvar++;
			}
			if (function[checkvar]!='('){
				newFunction+="(";
				newFunction+=function[checkvar];
				newFunction+=")";
			} else {
				newFunction+=function[checkvar];
			}
			i=checkvar;
		}
	}
	function = newFunction;
	newFunction = "";
	for (int i=0;i<function.length();i++){
		newFunction+=function[i];
		if (function[i]=='?'||function[i]==')'||function[i]==variable.at(0)){
			if (function.length()>i+1){
				if (function[i+1]=='?'||function[i+1]==variable.at(0)||function[i+1]=='('||function[i+1]=='_'){
					newFunction+="*";
				}
			}
		}
	}
	function = newFunction;
	newFunction = "";
	Constant* trav = head->next;
	for (int i=0;i<function.length();i++){
		if (function[i]=='('){
			if (function[i+1]=='$'&&function[i+3]==')'){
				if (function[i+2]==variable.at(0)){
					newFunction += "(";
					newFunction += function[i+2];
					newFunction += "$)";
				} else {
					newFunction += "(?)";
					if (trav->constant[trav->constant.length()-1]=='$'){
						trav->constant = trav->constant.substr(0, trav->constant.length()-1);
					} else {
						trav->constant += '$';
					}
				}
				i=i+3;
			} else if (function[i+1]=='$'&&function[i+3]=='$'&&function[i+4]==')'){
				newFunction += "(";
				newFunction += function[i+2];
				newFunction += ")";
				i=i+4;
			} else {
				newFunction += function[i];
			}
		} else {
			newFunction += function[i];
		}
	}
	//newFunction += function[function.length()-1];
	head->constant = newFunction;
	return head;
}

//This function uses the level struct to break up the function into the tree so that order of operations created by parentheses is satisfied
Level* breakInput (string function, int* i){
	Level* level = new Level();
	Level** nextLevelCurrent = &(level->nextLevel);
	for (;(*i)<function.length();(*i)++){
		if (function[(*i)]=='('){
			(*i)++;
		 	*nextLevelCurrent = breakInput(function, i);
			nextLevelCurrent = &((*nextLevelCurrent)->next);
			level->function += "@";
			(*i)--;
		} else if (function[(*i)]==')'){
			(*i)++;
			break;
		} else {
			level->function += function[(*i)];
		}
	}
	return level;
}

//Builds the function graph that is used for simplification
//Uses order of operations
//The same level (of same parentheses) operators are all in the same node
Algebra* buildFunctionGraph (Level* start, int startIL, string function, string variable){
	IndexOperator* head = NULL;
	IndexOperator* current = NULL;
	int smallestIndex = 0;
 	short smallest = 10;
	while (function=="@"){
		start = start->nextLevel;
		for (int i=0;i<startIL;i++){
			start = start->next;
		}
		function = start->function;
		startIL = 0;
	}
	int numOfAts = startIL;
	int prevNumOfAts = startIL;
	for (int i=0;i<function.length();i++){
		short temp = isOperator(function[i]);
		if (temp>-1){
			IndexOperator* index = new IndexOperator;
			index->index = i;
			index->operatorID = temp;
			index->split = prevNumOfAts;
			index->splitAfter = numOfAts;
			prevNumOfAts = numOfAts;
			index->next = NULL;
			if (head==NULL){
				head = index;
				current = index;
			} else {
				current->next = index;
				current = current->next;
			}
			if (temp<smallest){
				smallest = temp;
				smallestIndex = i;
			}
		} else if (function[i]=='@'){
			numOfAts++;
		}
	}
	IndexOperator* prev = NULL;
	current = head;
	while (current!=NULL){
		if (current->operatorID>smallest){
			IndexOperator* copy = current;
			if (prev!=NULL){
				if (current->next!=NULL){
					current->next->split = current->split;
				}
				prev->next = current->next;
			} else {
				int tempsplit = head->split;
				head = current->next;
				head->split = tempsplit;
			}
			delete copy;
		}
		prev = current;
		current = current->next;
	}
	current = head;
	if (current==NULL){
		int functionCheck = isFundamentalFunction(function.substr(0, 3));
		if (functionCheck>-1){
			AlgebraFundamentalFunction* aff = new AlgebraFundamentalFunction("_"+to_string(functionCheck));
			AlgebraList* templist = new AlgebraList;
			templist->algebra = buildFunctionGraph(start, startIL, function.substr(3), variable);
			templist->next = NULL;
			aff->connectedNodes = templist;
			return aff;
		} else {
			AlgebraFunction* af = new AlgebraFunction(function);
			return af;
		}
	} else {
		AlgebraOperator* ao = new AlgebraOperator(function.substr(current->index, 1));
		int first = -1;
		int last = 0;
		AlgebraList** trav = &(ao->connectedNodes);
		while (current!=NULL){
			last = current->index;
			AlgebraList* templist = new AlgebraList;
			templist->algebra = buildFunctionGraph(start, current->split, function.substr(first+1, last-first-1), variable);
			templist->next = NULL;
		  (*trav) = templist;
			trav = &((*trav)->next);
			first = current->index;
			numOfAts = current->splitAfter;
			current = current->next;
		}
		AlgebraList* templist = new AlgebraList;
		templist->algebra = buildFunctionGraph(start, numOfAts, function.substr(first+1), variable);
		templist->next = NULL;
		(*trav) = templist;
		return ao;
	}
	return NULL;
}

//Connects alike function nodes
Algebra* redirect (Algebra* start){
	if (isOperator(start->value[0])>-1){
		AlgebraList* trav1 = start->connectedNodes;
		AlgebraList* travprev = NULL;
		AlgebraList* trav2;
		AlgebraList* trav3;
		while (trav1!=NULL){
			if (isOperator(trav1->algebra->value[0])==isOperator(start->value[0])){
				trav2 = trav1->algebra->connectedNodes;
				if (travprev==NULL){
					start->connectedNodes = trav2;
				} else {
					travprev->next = trav2;
				}
				while (trav2->next!=NULL){
					trav2 = trav2->next;
				}
				trav2->next = trav1->next;
				delete trav1->algebra;
				delete trav1;
				trav1 = start->connectedNodes;
			} else {
				travprev = trav1;
				trav1 = trav1->next;
			}
		}
		trav1 = start->connectedNodes;
		while (trav1!=NULL){
			trav2=trav1->next;
			travprev = trav1;
			if (isOperator(trav1->algebra->value[0])>-1){
				trav1->algebra = redirect(trav1->algebra);
		 	}
			while (trav2!=NULL){
				if ((isdigit(trav1->algebra->value[0])&&isdigit(trav2->algebra->value[0]))||
				(isOperator(trav1->algebra->value[0])==-1&&trav1->algebra->value==trav2->algebra->value)){
					AlgebraList* temp = new AlgebraList;
					temp->algebra = trav2->algebra;
					temp->next = NULL;
					trav3 = trav1->algebra->connectedNodes;
					if (trav3==NULL){
						trav1->algebra->connectedNodes = temp;
					} else {
						while (trav3->algebra->connectedNodes!=NULL){
							trav3=trav3->algebra->connectedNodes;
						}
						trav3->algebra->connectedNodes = temp;
					}
					travprev->next = trav2->next;
				} else {
					travprev = trav2;
				}
				trav2 = trav2->next;
			}
			trav1 = trav1->next;
		}
	}
	return start;
}

//If function nodes are connected to eachother then calculate the connection using the operator the left node is connected to
Algebra* condenseSwitchConnected (Algebra* f, Algebra* g, string op, bool isNumber){
	if (isNumber){
		switch (op[0]){
			case('+'):
				f->value = to_string(stod(f->value) + stod(g->value));
				break;
			case('-'):
				f->value = to_string(stod(f->value) - stod(g->value));
			//NEGATIVE NUMBERS
				break;
			case('*'):
				f->value = to_string(stod(f->value) * stod(g->value));
				break;
			case('/'):
				f->value = to_string(stod(f->value) / stod(g->value));
				break;
			case('^'):
				f->value = to_string(pow(stod(f->value), stod(g->value)));
				break;
		}
	} else {
		switch (op[0]){
			case('+'):
				if (isdigit(f->value[0])==false){
					f->value = "1*"+f->value;
				}
				f->value = to_string(stoi(f->value.substr(0,f->value.find('*', 0)))+1)+g->value;
				break;
			case('-'):
				if (isdigit(f->value[0])==false){
					f->value = "1*"+f->value;
				}
				f->value = to_string(stoi(f->value.substr(0,f->value.find('*', 0)))-1)+g->value;
				break;
			case('*'):
				if (f->value.length()==1){
					f->value = f->value+"^1";
				}
				f->value = g->value+"^"+to_string(stoi(f->value.substr(2))+1);
				break;
			case('/'):
				break;
			case('^'):
				break;
		}
	}
	return f;
}

//If there are certain function nodes connected to specific operator nodes then do things
Algebra* condenseSwitchOperator (AlgebraList** connectedBy){
	AlgebraList** trav = &((*connectedBy)->algebra->connectedNodes);
	switch((*connectedBy)->algebra->value[0]){
		case('+'):
		break;
		case('-'):
		break;
		case('*'):
		{
			while (*trav!=NULL){
				if (isOperator((*trav)->algebra->value[0])==-1){
					if ((*trav)->algebra->value=="0"){
						trav = &((*connectedBy)->algebra->connectedNodes);
						while (*trav!=NULL){
							AlgebraList* temp1 = *trav;
							trav = &((*trav)->next);
							deleteGraphNode(temp1->algebra);
							delete temp1;
						}
						delete (*connectedBy)->algebra;
						(*connectedBy)->algebra = new AlgebraFunction("0");
						break;
					} else if ((*trav)->algebra->value=="1"){
						trav = &((*connectedBy)->algebra->connectedNodes);
						if (*trav!=NULL){
							while ((*trav)->next!=NULL){
								if ((*trav)->next->algebra->value=="1"){
									AlgebraList* temp1 = (*trav)->next;
									(*trav)->next = (*trav)->next->next;
									deleteGraphNode(temp1->algebra);
									delete temp1;
									break;
								}
							}
						}
					}
				}
				trav = &((*trav)->next);
			}
			break;
		}
		case('/'):
		{
			if (*trav!=NULL&&(*trav)->algebra->value=="0"){
				trav = &((*trav)->next);
				while (*trav!=NULL){
					AlgebraList* temp1 = *trav;
					AlgebraList** temp2 = trav;
					trav = &((*trav)->next);
					deleteGraphNode(temp1->algebra);
					delete temp1;
					*temp2 = NULL;
				}
			}
		}
		break;
		case('^'):
		break;
	}
}

//If there is an actual number in a fundamental function then run the calculation
string condenseSwitchFundamental (int function, string value, AlgebraList** g){
	switch (function){
		case(0):
		if (isdigit((*g)->algebra->value[0])){
			string temp = to_string(log(stod((*g)->algebra->value)));
			deleteGraphNode((*g)->algebra);
			delete *g;
			*g = NULL;
			return temp;
		} else if ((*g)->algebra->value[0]=='e'&&(*g)->algebra->value.length()==1){
			deleteGraphNode((*g)->algebra);
			delete *g;
			*g = NULL;
			return "1";
		} else {
			return value;
		}
		case(1):
		return value;
		case(2):
		return value;
		case(3):
		return value;
		case(4):
		return value;
		case(5):
		return value;
		case(6):
		return value;
	}
}

//Will be for similar operations such as 2*x + 3*x = 5*x
Algebra* condenseSwitchUnConnected (Algebra* f, Algebra* g, string opOn){
	// if (){
	//
	// }
}

//This runs all of the above functions in order on the graph in order to simplify the function
//The graph is rebuilt and recreated until the ingoing function is the same as the outgoing function
string simplify (string function, string variable){
	int simps = 1;
	int* i = new int (0);
	Level* start;
	Algebra* graph;
	string functionOld = "";
	while (function!=functionOld){
		cout<<"Iteration "+to_string(simps)<<endl;
		//cout<<"sdfdsfds"<<endl;
		start = breakInput(function, i);
		//cout<<"sdfdsfds1"<<endl;
		graph = buildFunctionGraph(start, 0, start->function, variable);
		//cout<<"sdfdsfds2"<<endl;
		graph = redirect(graph);
		//cout<<"sdfdsfds3"<<endl;
		graph->condense();
		//cout<<"sdfdsfds4"<<endl;
		AlgebraList* temp = new AlgebraList;
		temp->algebra = graph;
		condenseSwitchOperator(&temp);
		delete temp;
		functionOld = function;
		function = graph->getFunction();
		cout<<function<<endl;
		deleteGraphNode(graph);
		*i = 0;
		simps++;
	}
	delete i;
	return functionOld;
}

//Deletes a graph node and all of its connections
void deleteGraphNode (Algebra* node){
	AlgebraList* trav = node->connectedNodes;
	while (trav!=NULL){
		AlgebraList* temp = trav;
		if (isOperator(trav->algebra->value[0])==-1){
			delete trav->algebra;
			delete trav;
		} else {
			deleteGraphNode(temp->algebra);
			delete temp;
		}
		trav = trav->next;
	}
	delete node;
}
