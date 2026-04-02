# Implementation Guide: HAB Quiz Style Migration
## Match Quiz Appearance to the New White Jekyll Website Theme

**Repo:** `KristofM854/KristofM854.github.io`  
**Quiz source:** `quiz-src/` (React/Vite, built output deployed to `quiz/`)  
**Website:** `kristofmoeller.com` (Jekyll, white theme)  
**Prepared:** 2026-04-02

---

## CONTEXT

The HAB & Marine Biotoxins Quiz currently uses a dark ocean theme (`#0a0a0f` background, light text on dark cards, deep navy/teal/crimson category colors). The main website has been migrated to a clean white Jekyll theme with light backgrounds, dark text, and blue accent colors.

The quiz looks like it belongs to a completely different site. This guide brings the quiz's visual language in line with the website while keeping the quiz functional and readable.

---

## CRITICAL RULES

1. **Work in `quiz-src/`.** All style changes happen in the source code. After changes, rebuild and copy output to `quiz/`.
2. **Do not change quiz logic, questions, or functionality.** This is a visual-only refactor.
3. **Preserve category identity.** Each of the 6 categories should still be visually distinguishable, but using lighter tints instead of dark saturated backgrounds.
4. **Test on mobile.** The quiz is used on phones during workshops. Verify nothing breaks at narrow widths.

---

## STEP 1: Audit current quiz styles

Before changing anything, locate where styles are defined. The quiz is a React app — styles may be in:

- Inline `style={{...}}` objects in JSX components
- A CSS file (e.g., `quiz-src/src/index.css` or `quiz-src/src/App.css`)
- Tailwind classes (check if `tailwind.config.js` exists)
- A theme/constants file (e.g., category color definitions)

Run:
```bash
cd quiz-src
grep -rn "background" src/ --include="*.jsx" --include="*.tsx" --include="*.css" | head -30
grep -rn "#0a0a0f\|#0F172A\|#1E293B\|#0c1a2e" src/ | head -20
```

Identify the file(s) that define:
- The main page background color
- Category color mappings (the object that maps category names to `{color, bg, border, icon}`)
- Card/container background colors
- Text colors
- Button styles
- Progress bar colors
- The results/score screen colors

---

## STEP 2: Define the new palette

The Jekyll website uses these design tokens. The quiz should adopt the same system:

### Base colors (from Jekyll site CSS)
```
Page background:      #FFFFFF (white)
Card background:      #FFFFFF (white, with subtle border or shadow)
Card border:          #E2E8F0 (light grey)
Card hover shadow:    rgba(0, 0, 0, 0.08)
Text primary:         #1E293B (dark navy)
Text secondary:       #64748B (medium grey)
Text muted:           #94A3B8 (light grey)
Accent / links:       #2563EB (blue-600)
Accent hover:         #1D4ED8 (blue-700)
Success:              #16A34A (green-600)
Error / wrong:        #DC2626 (red-600)
Warning:              #D97706 (amber-600)
Border radius:        8px (cards), 6px (buttons)
```

### Category colors — LIGHT TINT VERSION
Each category gets a light-background tint instead of the current dark saturated background:

```javascript
const CATEGORY_STYLES = {
  "HAB Ecology & Species": {
    accent: "#16A34A",      // green-600
    bg: "#F0FDF4",          // green-50
    border: "#BBF7D0",      // green-200
    icon: "🌊"
  },
  "Marine Biotoxins": {
    accent: "#DC2626",      // red-600
    bg: "#FEF2F2",          // red-50
    border: "#FECACA",      // red-200
    icon: "☠️"
  },
  "Monitoring & Detection": {
    accent: "#2563EB",      // blue-600
    bg: "#EFF6FF",          // blue-50
    border: "#BFDBFE",      // blue-200
    icon: "🔬"
  },
  "Regulatory Frameworks": {
    accent: "#7C3AED",      // violet-600
    bg: "#F5F3FF",          // violet-50
    border: "#DDD6FE",      // violet-200
    icon: "📋"
  },
  "Human & Animal Health": {
    accent: "#D97706",      // amber-600
    bg: "#FFFBEB",          // amber-50
    border: "#FDE68A",      // amber-200
    icon: "🏥"
  },
  "Environmental Factors": {
    accent: "#0891B2",      // cyan-600
    bg: "#ECFEFF",          // cyan-50
    border: "#A5F3FC",      // cyan-200
    icon: "🌡️"
  }
};
```

Verify these category names match the actual names used in the quiz source code. Adjust if the source uses different strings.

---

## STEP 3: Update the main page background and typography

### Background
Find every occurrence of the dark background (`#0a0a0f`, `#0F172A`, or similar) applied to the root/body/main container and replace with white:

```
Old: background: "#0a0a0f"  or  background: "#0F172A"
New: background: "#FFFFFF"
```

If there is a `min-height: 100vh` wrapper div, update it:
```javascript
// Old
style={{ minHeight: "100vh", background: "#0a0a0f", color: "#e2e8f0" }}

// New
style={{ minHeight: "100vh", background: "#FFFFFF", color: "#1E293B" }}
```

### Typography
Update the font family to match the Jekyll site. The website uses a system font stack:

```
Old: fontFamily: "Georgia, serif"  (or whatever the quiz currently uses)
New: fontFamily: "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"
```

### Text colors
Replace all light-on-dark text colors:
```
Old #e2e8f0 / #f1f5f9 (light text for dark bg) → New #1E293B (dark text for white bg)
Old #94a3b8 / #64748b (muted text for dark bg) → New #64748B (stays same, works on white too)
Old #475569 (very muted on dark) → New #94A3B8 (muted on white)
```

---

## STEP 4: Update cards and containers

### Question cards
Replace dark card backgrounds with white cards that have a subtle border and shadow:

```javascript
// Old
style={{
  background: "#1E293B",
  border: "1px solid #334155",
  borderRadius: "8px",
  padding: "1.5rem"
}}

// New
style={{
  background: "#FFFFFF",
  border: "1px solid #E2E8F0",
  borderRadius: "8px",
  padding: "1.5rem",
  boxShadow: "0 1px 3px rgba(0, 0, 0, 0.06)"
}}
```

### Answer option buttons/cards
Replace the dark interactive elements with light ones:

```javascript
// Old (unselected answer)
style={{
  background: "#0F172A",
  border: "1px solid #334155",
  color: "#e2e8f0",
  ...
}}

// New (unselected answer)
style={{
  background: "#F8FAFC",
  border: "1px solid #E2E8F0",
  color: "#1E293B",
  cursor: "pointer",
  transition: "all 0.15s ease",
  ...
}}

// New (hover state — add via CSS or onMouseEnter)
// background: "#F1F5F9", borderColor: "#CBD5E1"

// New (selected answer)
// background: "#EFF6FF", border: "2px solid #2563EB", color: "#1E293B"

// New (correct answer revealed)
// background: "#F0FDF4", border: "2px solid #16A34A", color: "#1E293B"

// New (wrong answer revealed)
// background: "#FEF2F2", border: "2px solid #DC2626", color: "#1E293B"
```

---

## STEP 5: Update category headers and badges

The category indicator (shown above each question or on the category selection screen) should use the light tint palette from Step 2:

```javascript
// Old category badge
style={{
  background: category.bg,  // was e.g. "#0c2a1e" (dark green)
  border: `1px solid ${category.border}`,
  color: category.color,    // was e.g. "#4ade80" (bright green)
  ...
}}

// New category badge
style={{
  background: category.bg,     // now "#F0FDF4" (green-50)
  border: `1px solid ${category.border}`,  // now "#BBF7D0" (green-200)
  color: category.accent,      // now "#16A34A" (green-600)
  padding: "4px 12px",
  borderRadius: "999px",       // pill shape
  fontSize: "0.8125rem",
  fontWeight: "600",
  ...
}}
```

---

## STEP 6: Update buttons

### Primary action button (Start Quiz, Next Question, etc.)
```javascript
// New primary button
style={{
  background: "#2563EB",
  color: "#FFFFFF",
  border: "none",
  borderRadius: "6px",
  padding: "10px 24px",
  fontSize: "0.9375rem",
  fontWeight: "600",
  cursor: "pointer",
  transition: "background 0.15s ease",
  // hover: background: "#1D4ED8"
}}
```

### Secondary button (Back, Skip, etc.)
```javascript
style={{
  background: "transparent",
  color: "#2563EB",
  border: "1px solid #2563EB",
  borderRadius: "6px",
  padding: "10px 24px",
  fontSize: "0.9375rem",
  fontWeight: "600",
  cursor: "pointer",
  // hover: background: "#EFF6FF"
}}
```

---

## STEP 7: Update progress bar

```javascript
// Old (bright colored bar on dark background)
// Track: "#1E293B", fill: "#4ade80"

// New
// Track: "#E2E8F0" (grey-200), fill: "#2563EB" (blue-600)

// Progress bar track
style={{
  background: "#E2E8F0",
  borderRadius: "999px",
  height: "8px",
  overflow: "hidden"
}}

// Progress bar fill
style={{
  background: "#2563EB",
  height: "100%",
  borderRadius: "999px",
  transition: "width 0.3s ease"
}}
```

---

## STEP 8: Update results/score screen

The final score screen should match the white theme:

```javascript
// Score circle or percentage display
style={{
  color: "#1E293B",
  fontSize: "3rem",
  fontWeight: "700"
}}

// Score label
style={{
  color: "#64748B",
  fontSize: "0.875rem"
}}

// Score context (pass/fail messaging)
// Use green-600 (#16A34A) for good scores, amber (#D97706) for mediocre, red (#DC2626) for low

// Category breakdown bars — use each category's accent color on a light track
```

---

## STEP 9: Update the header/title area

```javascript
// Old
// Dark background with light text, serif font

// New — match website's clean look
style={{
  textAlign: "center",
  marginBottom: "2rem",
  paddingTop: "1rem"
}}

// Title
style={{
  fontSize: "1.75rem",
  fontWeight: "700",
  color: "#1E293B",
  margin: "0 0 0.25rem 0"
}}

// Subtitle
style={{
  fontSize: "0.875rem",
  color: "#64748B"
}}
```

---

## STEP 10: Update any remaining dark elements

Search for all remaining dark color values and replace:

```bash
cd quiz-src
# Find any remaining dark backgrounds
grep -rn "#0a0a0f\|#0F172A\|#1E293B\|#334155\|#475569" src/
# Find any remaining light-on-dark text
grep -rn "#e2e8f0\|#f1f5f9\|#cbd5e1" src/
```

Also check for:
- Tooltip backgrounds (should be `#1E293B` text on `#FFFFFF` bg with border)
- Modal/overlay backgrounds (use `rgba(0, 0, 0, 0.3)` backdrop)
- Divider/separator lines (use `#E2E8F0`)
- Explanation text boxes (after answering a question, if there are fact boxes — use the category tint as background)
- Share buttons on results screen
- The "Back to site" link if one exists

---

## STEP 11: Add subtle page structure

To make the quiz feel embedded in the website rather than standalone, consider adding a thin top border or minimal header that echoes the site nav:

```javascript
// Optional: thin accent line at top of page
<div style={{
  height: "3px",
  background: "linear-gradient(90deg, #2563EB, #7C3AED, #0891B2)",
  width: "100%"
}} />
```

And a subtle footer link back to the main site:

```javascript
<div style={{
  textAlign: "center",
  padding: "2rem 0 1rem",
  fontSize: "0.8125rem",
  color: "#94A3B8"
}}>
  <a href="/" style={{ color: "#2563EB", textDecoration: "none" }}>
    ← Back to kristofmoeller.com
  </a>
</div>
```

---

## STEP 12: Build and deploy

```bash
cd quiz-src
npm install
npm run build
# Copy build output to quiz/
# (check package.json for the actual build output directory — likely dist/)
rm -rf ../quiz/*
cp -r dist/* ../quiz/
```

Verify locally:
```bash
cd ..
# If using Jekyll locally:
bundle exec jekyll serve
# Visit http://localhost:4000/quiz/
```

Check:
- [ ] White background throughout, no dark panels remaining
- [ ] All text readable (dark on light, not light on light)
- [ ] Category colors visible as tinted badges/accents
- [ ] Correct/incorrect answer states clearly distinguishable
- [ ] Progress bar visible on white background
- [ ] Results screen readable
- [ ] Mobile layout not broken (test at 375px width)
- [ ] No console errors

---

## SUMMARY OF COLOR MAPPINGS

| Element | Old (Dark Theme) | New (White Theme) |
|---------|-----------------|-------------------|
| Page background | `#0a0a0f` / `#0F172A` | `#FFFFFF` |
| Card background | `#1E293B` | `#FFFFFF` + border `#E2E8F0` |
| Primary text | `#e2e8f0` / `#f1f5f9` | `#1E293B` |
| Secondary text | `#94a3b8` | `#64748B` |
| Muted text | `#64748b` | `#94A3B8` |
| Accent | varies | `#2563EB` |
| Borders | `#334155` | `#E2E8F0` |
| Answer hover | `#334155` bg | `#F1F5F9` bg |
| Correct state | bright green on dark | `#F0FDF4` bg + `#16A34A` border |
| Wrong state | bright red on dark | `#FEF2F2` bg + `#DC2626` border |
| Progress track | `#1E293B` | `#E2E8F0` |
| Progress fill | `#4ade80` or category color | `#2563EB` |
| Category badges | dark saturated bg + bright text | light tint bg + dark accent text |
| Font | Georgia, serif | System sans-serif stack |
