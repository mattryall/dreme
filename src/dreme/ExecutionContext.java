package dreme;

public interface ExecutionContext 
{
	Environment getEnvironment();
	void returnValue(SchemeObject returnValue);
	void returnLastResult();
	List evaluatedValues();
	void execute(List executable, Environment environment);
	void executeInPlace(List executable, Environment environment);
	List getRawBody();
	void addResult(SchemeObject result);

    ExecutionContext copy();

    void replaceWith(ExecutionContext context);
}

