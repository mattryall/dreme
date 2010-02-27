package dreme.runtime;

import dreme.ExecutionContext;
import dreme.List;
import dreme.Operator;
import dreme.SchemeObject;
import dreme.macros.Lambda;

class CallCCOperator extends dreme.BasicSchemeObject implements Operator {
    public void apply(ExecutionContext context) {
        SchemeObject parameter = context.evaluatedValues().head();
        if (!(parameter instanceof Lambda))
            throw new IllegalArgumentException("call/cc parameter must be a lambda. Got a " + parameter);
        List ccExecutionList = new List();
        ccExecutionList.add(parameter);
        ccExecutionList.add(new ContinuationResumeOperator(context.copy()));
        context.executeInPlace(ccExecutionList, context.getEnvironment());
    }

    private static class ContinuationResumeOperator extends dreme.BasicSchemeObject implements Operator {
        private final ExecutionContext context;

        public ContinuationResumeOperator(ExecutionContext context) {
            this.context = context;
        }

        public void apply(ExecutionContext context) {
            SchemeObject continuationReturn = context.evaluatedValues().head();
            context.replaceWith(this.context);
            context.returnValue(continuationReturn);
        }
    }
}
