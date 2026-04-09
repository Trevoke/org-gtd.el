---
name: code-review
description: Use after implementation to review code against spec and quality standards
---

# Code Review

Review implementation against spec and quality standards. Catch issues before they cascade.

## Review Process

### 1. Understand Context

- What was the task/bead requirement?
- What commits are being reviewed?
- What's the expected behavior?

### 2. Spec Compliance Review

Check that implementation matches requirements:

**Must verify:**
- [ ] All required features implemented
- [ ] No extra features added (YAGNI)
- [ ] Behavior matches spec exactly
- [ ] Edge cases handled as specified

**Red flags:**
- "I added X because it seemed useful" - reject
- Missing required functionality - reject
- Behavior differs from spec - reject

### 3. Code Quality Review

**Check for:**
- [ ] Tests exist and are meaningful
- [ ] Tests actually pass
- [ ] Code is readable and maintainable
- [ ] No obvious bugs or security issues
- [ ] No magic numbers or hardcoded values
- [ ] Error handling is appropriate

**Severity levels:**
- **Critical** - Must fix before proceeding (bugs, security, broken tests)
- **Important** - Should fix before proceeding (missing tests, poor structure)
- **Minor** - Can note for later (style, minor improvements)

### 4. Provide Feedback

Structure your review:

```
## Spec Compliance
[✅ Compliant / ❌ Issues found]

Issues (if any):
- Missing: [what's missing from spec]
- Extra: [what was added but not requested]

## Code Quality

Strengths:
- [What's done well]

Issues:
- [Critical] [issue description]
- [Important] [issue description]
- [Minor] [issue description]

## Assessment
[Ready to proceed / Needs fixes]
```

## Acting on Feedback

**If reviewer finds issues:**
1. Implementer fixes the issues
2. Reviewer reviews again
3. Repeat until approved

**If you disagree with feedback:**
- Push back with technical reasoning
- Show code/tests that prove correctness
- Request clarification if unclear

## Red Flags

**Never:**
- Skip review because "it's simple"
- Ignore Critical issues
- Proceed with unfixed Important issues
- Rubber-stamp without actually reviewing

**Always:**
- Check spec compliance first
- Run the tests yourself
- Verify claimed fixes actually work
