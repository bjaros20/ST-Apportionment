#ssfa_plot

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import geopandas as gpd
from matplotlib.patches import Rectangle
from matplotlib.lines import Line2D
import pandas as pd
from mpl_toolkits.axes_grid1.inset_locator import inset_axes

# State data with SSF adoption years
ssf_data = {
    # 1970s
    'Iowa': {'year': 1978, 'decade': '1970s'},
    # 1980s
    'Nebraska': {'year': 1988, 'decade': '1980s'},
    # 1990s
    'Michigan': {'year': 1991, 'decade': '1990s'},
    'Illinois': {'year': 1999, 'decade': '1990s'},
    # 2000s
    'Oregon': {'year': 2004, 'decade': '2000s'},
    'Georgia': {'year': 2006, 'decade': '2000s'},
    'Wisconsin': {'year': 2006, 'decade': '2000s'},
    'Arizona': {'year': 2007, 'decade': '2000s'},
    'Indiana': {'year': 2007, 'decade': '2000s'},
    'Maine': {'year': 2007, 'decade': '2000s'},
    'Minnesota': {'year': 2007, 'decade': '2000s'},
    'Pennsylvania': {'year': 2007, 'decade': '2000s'},
    'South Carolina': {'year': 2007, 'decade': '2000s'},
    'Colorado': {'year': 2009, 'decade': '2000s'},
    # 2010s
    'California': {'year': 2011, 'decade': '2010s'},
    'Utah': {'year': 2011, 'decade': '2010s'},
    'New Jersey': {'year': 2012, 'decade': '2010s'},
    'New York': {'year': 2015, 'decade': '2010s'},
    'Rhode Island': {'year': 2015, 'decade': '2010s'},
    'Connecticut': {'year': 2016, 'decade': '2010s'},
    'Louisiana': {'year': 2016, 'decade': '2010s'},
    'North Carolina': {'year': 2016, 'decade': '2010s'},
    'North Dakota': {'year': 2016, 'decade': '2010s'},
    'Delaware': {'year': 2017, 'decade': '2010s'},
    'Kentucky': {'year': 2018, 'decade': '2010s'},
    'Maryland': {'year': 2018, 'decade': '2010s'},
    # 2020s
    'Missouri': {'year': 2020, 'decade': '2020s'},
    'Alabama': {'year': 2021, 'decade': '2020s'},
    'Arkansas': {'year': 2021, 'decade': '2020s'},
    'Idaho': {'year': 2022, 'decade': '2020s'},
    'New Hampshire': {'year': 2022, 'decade': '2020s'},
    'West Virginia': {'year': 2022, 'decade': '2020s'},
    'Vermont': {'year': 2023, 'decade': '2020s'},
    'Tennessee': {'year': 2024, 'decade': '2020s'},
    'Massachusetts': {'year': 2025, 'decade': '2020s'},
    'Montana': {'year': 2025, 'decade': '2020s'}
}

# States with corporate tax but no SSF
corp_tax_no_ssf = ['Alaska', 'Hawaii', 'Kansas', 'Oklahoma', 'New Mexico', 
                   'Florida', 'Mississippi', 'Virginia']

# States with no corporate income tax
no_corporate_tax = ['Nevada', 'South Dakota', 'Texas', 'Washington', 'Wyoming', 'Ohio']

# Color scheme - Professional blues that convert well to grayscale
color_scheme = {
    '1970s': '#2171b5',  # Darkest blue (prints as dark gray)
    '1980s': '#2171b5',  # Dark blue (prints as dark gray)
    '1990s': '#2171b5',  # Medium-dark blue (prints as medium-dark gray)
    '2000s': '#4292c6',  # Medium blue (prints as medium gray)
    '2010s': '#6baed6',  # Light-medium blue (prints as light-medium gray)
    '2020s': '#9ecae1',  # Light blue (prints as light gray)
    'corp_tax_no_ssf': '#deebf7',  # Very light blue (prints as very light gray)
    'no_corporate_tax': '#ffffff'  # White
}

# State abbreviations
state_abbr = {
    'Alabama': 'AL', 'Alaska': 'AK', 'Arizona': 'AZ', 'Arkansas': 'AR', 'California': 'CA',
    'Colorado': 'CO', 'Connecticut': 'CT', 'Delaware': 'DE', 'Florida': 'FL', 'Georgia': 'GA',
    'Hawaii': 'HI', 'Idaho': 'ID', 'Illinois': 'IL', 'Indiana': 'IN', 'Iowa': 'IA',
    'Kansas': 'KS', 'Kentucky': 'KY', 'Louisiana': 'LA', 'Maine': 'ME', 'Maryland': 'MD',
    'Massachusetts': 'MA', 'Michigan': 'MI', 'Minnesota': 'MN', 'Mississippi': 'MS', 'Missouri': 'MO',
    'Montana': 'MT', 'Nebraska': 'NE', 'Nevada': 'NV', 'New Hampshire': 'NH', 'New Jersey': 'NJ',
    'New Mexico': 'NM', 'New York': 'NY', 'North Carolina': 'NC', 'North Dakota': 'ND', 'Ohio': 'OH',
    'Oklahoma': 'OK', 'Oregon': 'OR', 'Pennsylvania': 'PA', 'Rhode Island': 'RI', 'South Carolina': 'SC',
    'South Dakota': 'SD', 'Tennessee': 'TN', 'Texas': 'TX', 'Utah': 'UT', 'Vermont': 'VT',
    'Virginia': 'VA', 'Washington': 'WA', 'West Virginia': 'WV', 'Wisconsin': 'WI', 'Wyoming': 'WY',
    'District of Columbia': 'DC', 'Puerto Rico': 'PR'
}

# States that need callout boxes
callout_states = ['Maine', 'New Hampshire', 'Vermont', 'Massachusetts', 
                  'Rhode Island', 'Connecticut', 'New Jersey', 'Delaware', 'Maryland']

# Load US states shapefile
# Note: You'll need to download a US states shapefile. You can get one from:
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

try:
    # Try to load from a local shapefile first
    states = gpd.read_file('states_shapefile/cb_2023_us_state_20m.shp')
except:
    # If local file not found, use Natural Earth data
    print("Loading state boundaries from Natural Earth...")
    states = gpd.read_file('https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_1_states_provinces.zip')
    # Filter for US states only
 #   states = states[states['admin'] == 'United States of America']
  #  states = states[~states['name'].isin(['United States Virgin Islands', 'Northern Mariana Islands', 
   #                                       'American Samoa', 'Guam'])]

# Create color mapping for states
def get_state_color(state_name):
    if state_name in ssf_data:
        return color_scheme[ssf_data[state_name]['decade']]
    elif state_name in corp_tax_no_ssf:
        return color_scheme['corp_tax_no_ssf']
    elif state_name in no_corporate_tax:
        return color_scheme['no_corporate_tax']
    else:
        return color_scheme['corp_tax_no_ssf']

# Apply colors to states
states['color'] = states['NAME'].apply(get_state_color)

# Create the figure
fig, ax = plt.subplots(1, 1, figsize=(18, 12))
ax.set_aspect('equal')

# Plot states
states.plot(ax=ax, color=states['color'], edgecolor='black', linewidth=0.5)

# ADD THE NEW CODE HERE (right after the states.plot line):
# Create insets for Alaska and Hawaii
# CHANGE 5: Alaska inset - make smaller
ax_alaska = inset_axes(ax, width="20%", height="20%", loc='lower left', 
                       bbox_to_anchor=(0.01, 0.01, 1, 1), bbox_transform=ax.transAxes)
alaska = states[states['NAME'] == 'Alaska']
alaska.plot(ax=ax_alaska, color=alaska['color'], edgecolor='black', linewidth=0.5)
ax_alaska.set_xlim(-180, -125)
ax_alaska.set_ylim(52, 72)
ax_alaska.set_xticks([])
ax_alaska.set_yticks([])
ax_alaska.set_title('Alaska', fontsize=14)

# CHANGE 5: Hawaii inset - make smaller
ax_hawaii = inset_axes(ax, width="15%", height="15%", loc='lower left',
                       bbox_to_anchor=(0.22, 0.01, 1, 1), bbox_transform=ax.transAxes)
hawaii = states[states['NAME'] == 'Hawaii']
hawaii.plot(ax=ax_hawaii, color=hawaii['color'], edgecolor='black', linewidth=0.5)
ax_hawaii.set_xlim(-162, -154)
ax_hawaii.set_ylim(18, 23)
ax_hawaii.set_xticks([])
ax_hawaii.set_yticks([])
ax_hawaii.set_title('Hawaii', fontsize=14)

# Add labels for larger states (excluding callout states)
for idx, row in states.iterrows():
    state_name = row['NAME']
    if state_name in ssf_data and state_name not in callout_states:
        # Get centroid of the state
        centroid = row['geometry'].centroid
        # Check if state is large enough for label
        bounds = row['geometry'].bounds
        width = bounds[2] - bounds[0]
        height = bounds[3] - bounds[1]
        
        # CHANGE 6: Only label larger states - adjust threshold for Tennessee
        if width > 2 and height > 2 or state_name == 'Tennessee':
            # Use black text for white states
            text_color = 'black' if state_name in ['Iowa', 'Nebraska', 'Michigan', 'Illinois'] else 'black'
            ax.text(centroid.x, centroid.y, str(ssf_data[state_name]['year']), 
                   ha='center', va='center', fontsize=14, fontweight='bold', color=text_color)
            
# Use white text for 1970s-1990s states (which are now white)
if width > 2 and height > 2 or state_name == 'Tennessee':
    # Use white text for 1970s-1990s states, black for others
    if state_name in ['Iowa', 'Nebraska', 'Michigan', 'Illinois']:
        text_color = 'white'
    else:
        text_color = 'black'
    ax.text(centroid.x, centroid.y, str(ssf_data[state_name]['year']), 
           ha='center', va='center', fontsize=14, fontweight='bold', color=text_color)


# Add callout boxes for northeastern states
callout_positions = {
    'Maine': (-65, 45),
    'New Hampshire': (-65, 44),
    'Vermont': (-65, 43),
    'Massachusetts': (-65, 42),
    'Rhode Island': (-65, 41),
    'Connecticut': (-65, 40),
    'New Jersey': (-65, 39),
    'Delaware': (-65, 38),
    'Maryland': (-65, 37)
}

for state, (box_x, box_y) in callout_positions.items():
    if state in ssf_data:
        # Get state centroid
        state_row = states[states['NAME'] == state]
        if not state_row.empty:
            centroid = state_row.geometry.iloc[0].centroid
            
            # CHANGE 7: Draw line from state to edge of box (not center)
            line_end_x = box_x - 2  # Left edge of box
            ax.plot([centroid.x, line_end_x], [centroid.y, box_y], 
                   'k-', linewidth=0.5, alpha=0.5)
            
            # Draw box
            box = Rectangle((box_x - 2, box_y - 0.3), 4, 0.6, 
                          fill=True, facecolor='white', 
                          edgecolor='black', linewidth=0.5)
            ax.add_patch(box)
            
            # Add text
            ax.text(box_x, box_y, f"{state_abbr[state]} {ssf_data[state]['year']}", 
                   ha='center', va='center', fontsize=14)

# Set map extent (continental US + space for callouts)
ax.set_xlim(-130, -60)
ax.set_ylim(24, 50)

# Remove axes
ax.set_xticks([])
ax.set_yticks([])
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['bottom'].set_visible(False)
ax.spines['left'].set_visible(False)

# Add title at the very top of the figure
fig.text(0.5, 0.95, 'Single Sales Factor Apportionment Adoption in the United States (1978-2025)', 
         ha='center', va='center', fontsize=20, fontweight='bold')

# CHANGE 3: Remove subtitle - commented out
# ax.text(0.5, 0.95, '36 states have adopted single sales factor apportionment for corporate income tax (1978-2025)',
#         transform=ax.transAxes, ha='center', fontsize=12, color='gray')

# CHANGE 4: Create legend with combined 1970s-1990s
legend_elements = [
    mpatches.Patch(color=color_scheme['1970s'], edgecolor='black', linewidth=0.5, label='1970s-1990s (4 states)'),
    mpatches.Patch(color=color_scheme['2000s'], label='2000s (10 states)'),
    mpatches.Patch(color=color_scheme['2010s'], label='2010s (12 states)'),
    mpatches.Patch(color=color_scheme['2020s'], label='2020s (10 states)'),
    mpatches.Patch(color=color_scheme['corp_tax_no_ssf'], label='Corp. Inc. Tax, No SSFA (8 states)'),
    mpatches.Patch(color=color_scheme['no_corporate_tax'], edgecolor='black', linewidth=0.5, label='No corp. income tax (6 states)')
]

ax.legend(handles=legend_elements, loc='lower center', bbox_to_anchor=(0.5, -0.08),
          ncol=3, frameon=False, fontsize=11)

# Add note
ax.text(0.5, -0.25, 'Note: This map shows the temporal and geographic spread of single sales factor adoption.\n' +
                    'States with corporate income tax but no SSF are shown in light gray. ' +
                    'States with no corporate income tax are shown in white.',
        transform=ax.transAxes, ha='center', fontsize=10, color='gray')

# CHANGE 8: Adjust layout to prevent clipping and add more top space
# Adjust layout to prevent clipping and make room for title
plt.tight_layout()
plt.subplots_adjust(top=0.90, bottom=0.15)  # Make room at top for title

# Save the map in multiple formats
plt.savefig('ssf_adoption_map5.png', dpi=300, bbox_inches='tight', facecolor='white')

print("Map saved as:")
print("- ssf_adoption_map5.png (for presentations)")

# Display the map
plt.show()