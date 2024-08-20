# Scrape job postings from [CESNET-L](https://www.cesnet-l.net/)
# Emilio Lehoucq - 8/16/24
# Script to run in Northwestern's HPC cluster (Quest)

##################################### Importing libraries #####################################
import argparse
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.options import Options
from bs4 import BeautifulSoup
from datetime import datetime
from time import sleep
from pandas import DataFrame
import os
from dotenv import load_dotenv
import logging
from datetime import datetime
from text_extractor import extract_text
import numpy as np

##################################### Parsing arguments #####################################

def parse_commandline():
    """Parse the arguments given on the command-line.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input-argument-1",
                       help="Input argument 1")
    args = parser.parse_args()
    return args

# QUEST UNCOMMENT
compilation_url = parse_commandline().input_argument_1 # This is a string containing a given row in inputs.txt

# LOCAL MACHINE UNCOMMENT
# # Open the .txt file and read the first line
# with open('inputs.txt', 'r') as file:
#     # Read the first line in the file
#     compilation_url = file.readline().strip()

##################################### Configure the logging settings #####################################

# Current timestamp
ts = datetime.now().strftime("%Y_%m_%d_%H_%M_%S_%f")

# Unique id for each compilation url
url_id = compilation_url[47:52]

# Defining path for log file
# QUEST UNCOMMENT
log_filename = '/projects/p32130/gideon/python_logs/'+ts+'_'+url_id+'.log'
# LOCAL MACHINE UNCOMMENT
# log_filename = 'test.log'

# Configure the logging settings
logging.basicConfig(filename=log_filename, level=logging.DEBUG, format='%(asctime)s -- %(message)s\n')
logging.info(f"Compilation URL: {compilation_url}")
logging.info(f"Compilation URL ID: {url_id}")

##################################### Sleep some time before starting #####################################
# Doing this to avoid overloading the website. I want a multimodal distribution for the sleep duration

# Define means and standard deviations for the Gaussians
means = [30, 90, 150, 210, 270, 330, 390]
std_devs = [0.01]*len(means)

# Choose a random mode (Gaussian)
mode = np.random.choice(len(means))

# Generate a random duration from the selected Gaussian
# Take the absolute value of the sleep duration to ensure it's positive (even if very unlikely)
sleep_duration = abs(np.random.normal(means[mode], std_devs[mode]))

# Sleep for the generated duration
sleep(sleep_duration)
logging.info(f"Slept for {sleep_duration} seconds before starting.")

##################################### Defining functions for this script #####################################

# I'm not turning the next three functions into a class with three methods because it doesn't work within
# soup_compilation.find_all('a', href = True, string = Class.method)
def contains_posting(text):
    '''
    Function to filter HTML <a> elements containing postings.

    Input: text in an <a> element.
    Outupt: boolean--True if contains posting, False otherwise.
    '''
    return text and ('faculty' in text.lower() or 'professor' in text.lower() or 'position' in text.lower() or 'instructor' in text.lower())

def contains_plain_text(text):
    '''
    Function to filter HTML <a> elements containing 'text/plain'.

    Input: text in an <a> element.
    Outupt: boolean--True if contains 'text/plain', False otherwise.
    '''
    return text and ('text/plain' in text.lower())

def contains_html(text):
    '''
    Function to filter HTML <a> elements containing 'text/html'.

    Input: text in an <a> element.
    Outupt: boolean--True if contains 'text/html', False otherwise.
    '''
    return text and ('text/html' in text.lower())

def login_cesnet(driver, password):
    """
    Function to log in to the CESNET-L website.

    Inputs:
    - driver: the web driver.
    - password: the password to log in to the website.
    Output: None.

    Dependencies: selenium.webdriver, logging
    """
    # Log in to the website
    # Find password input field and insert password
    driver.find_element("id", "Password").send_keys(password)
    logging.info("Inside login_cesnet: password inserted.")

    # Click log in button
    driver.find_element("name", "e").click()
    logging.info("Inside login_cesnet: clicked log in button.")

def check_login_required(source_code):
    """
    Function to check if the source code indicates that a login is required.

    Input: source code of a webpage.
    Output: boolean--True if login is required, False otherwise.

    Dependencies: logging
    """
    login_text = 'Please enter your email address and your LISTSERV password and click on the "Log In" button.'
    logging.info(f"Inside check_login_required: returning {login_text in source_code}.")
    return login_text in source_code

logging.info('Functions defined.')

##################################### Setting parameters #####################################

# Define https://listserv.kent.edu/ credentials
load_dotenv()
password = os.getenv('PASSWORD')
logging.info("Password obtained.")

# Set sleep time (this is a lot, but the website is slow sometimes)
sleep_time = 15

# Set number of tries
ntries = 15

# Set a delay between retries
retry_delay = 15

# Define URL base
url_base = 'https://listserv.kent.edu'

logging.info('All parameters set.')

##################################### Scrape #####################################

# Create list to store the data for the weekly compilation
data_compilation = []

# Initialize the web driver
# Chrome doesn't currently work in Quest, potentially look into: Is Firefox particularly slow? Is there a way to speed it up?
options = Options()
options.add_argument("--headless") # QUEST UNCOMMENT
driver = webdriver.Firefox(options=options)
logging.info("Web driver initialized.")

# Retry block in case of failure
for attempt in range(ntries):

    logging.info(f"First re-try block. Attempt {attempt + 1}.")

    try:
        # Go to the compilation URL
        driver.get(compilation_url)
        logging.info(f"Web driver went to the compilation URL: {compilation_url}.")
        sleep(sleep_time)

        # Log in to the website
        login_cesnet(driver, password)
        sleep(sleep_time)

        # Get the source code for the compilation
        source_code = driver.page_source
        logging.info("Source code for the compilation obtained.")

        # Parse the source code for the compilation
        soup_compilation = BeautifulSoup(source_code, 'html.parser')
        logging.info("Source code for the compilation parsed.")

        # Get the week of the compilation (the second h2 element)
        week = soup_compilation.find_all('h2')[1].text.strip()  # e.g., August 2024, Week 3
        logging.info(f"Week of the compilation obtained: {week}.")

        # Find the URLs for the postings
        urls = [a['href'] for a in soup_compilation.find_all('a', href=True, string=contains_posting) if 'https' in a['href']]
        logging.info("URLs for the postings obtained.")

        # Break the loop if successful
        logging.info("First re-try block successful. About to break the loop.")
        break

    except Exception as e:
        logging.info(f"First re-try block. Attempt {attempt + 1} failed. Error: {e}")

        if attempt < ntries - 1:  # Check if we have retries left
            logging.info("First re-try block. Sleeping before retry.")
            sleep(retry_delay)
        else:
            logging.info("First re-try block. All retries exhausted.")
            raise  # Re-raise the last exception if all retries are exhausted

# Iterate over the URLs for the postings
for url in urls:
    logging.info(f"Starting loop for the URLs for the postings. URL: {url}.")

    # Create dictionary to store the data for the posting
    data_posting = {}

    # Store the week of the compilation
    data_posting['week'] = week

    # Store the URL for the compilation
    data_posting['url_compilation'] = compilation_url

    # Store the URL for the posting
    data_posting['url_posting'] = url

    # Store the current timestamp
    data_posting['timestamp'] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    logging.info("Data for the posting initialized.")

    # Retry block in case of failure
    for attempt in range(ntries):
        logging.info(f"Second re-try block. Attempt {attempt + 1}.")

        try:
            # Go to the URL of the posting
            driver.get(url)
            logging.info("Web driver went to the URL of the posting.")
            sleep(sleep_time)

            # Check if login is required
            if check_login_required(driver.page_source):
                logging.info("Login required. Logging in.")
                # Log in to the website
                login_cesnet(driver, password)
                sleep(sleep_time)

            # Get the source code for the posting
            source_code_posting = driver.page_source
            logging.info("Source code for the posting obtained.")

            # Parse the source code for the posting
            soup_posting = BeautifulSoup(source_code_posting, 'html.parser')
            logging.info("Source code for the posting parsed.")

            # Try getting the plain text data
            try:
                logging.info("Trying to find the plain text message.")

                # Find the URL of the plain text message
                url_message = url_base + soup_posting.find('a', href = True, string = contains_plain_text)['href']
                logging.info(f"URL for the plain text message obtained: {url_message}.")

                # Go to the message URL
                driver.get(url_message)
                logging.info("Web driver went to the message URL.")
                sleep(sleep_time)

                # Check if login is required
                if check_login_required(driver.page_source):
                    logging.info("Login required. Logging in.")
                    # Log in to the website
                    login_cesnet(driver, password)
                    sleep(sleep_time)

                # Get the source code for the message
                source_code_message = driver.page_source
                logging.info("Source code for the message obtained.")

                # Store the source code for the message
                data_posting['source_code_message'] = source_code_message
                logging.info("Source code for the message stored.")

                # Extract the text from the source code of the message
                text = extract_text(source_code_message)
                logging.info("Text for the message extracted.")

                # Store the text for the message
                data_posting['text_message'] = text
                logging.info("Text for the message stored.")

            except:
                logging.info("Something went wrong with finding the plain text message. Trying with the HTML message.")

                # Get the HTML data (if there's no plain text, there's HTML)
                # Pretty much the same as for the plain text. Not creating a function since it's only two. Maybe I should though...
                # Find the URL of the HTML message
                url_message = url_base + soup_posting.find('a', href = True, string = contains_html)['href']
                logging.info(f"URL for the HTML message obtained: {url_message}.")

                # Go to the message URL
                driver.get(url_message)
                logging.info("Web driver went to the message URL.")
                sleep(sleep_time)

                # Check if login is required
                if check_login_required(driver.page_source):
                    logging.info("Login required. Logging in.")
                    # Log in to the website
                    login_cesnet(driver, password)
                    sleep(sleep_time)

                # Get the source code for the message
                source_code_message = driver.page_source
                logging.info("Source code for the message obtained.")

                # Store the source code for the message
                data_posting['source_code_message'] = source_code_message
                logging.info("Source code for the message stored.")

                # Extract the text from the source code of the message
                text = extract_text(source_code_message)
                logging.info("Text for the message extracted.")

                # Store the text for the message
                data_posting['text_message'] = text
                logging.info("Text for the message stored.")

            # Break the loop if successful
            logging.info("Second re-try block successful. About to break the loop.")
            break

        except Exception as e:
            logging.info(f"Second re-try block. Attempt {attempt + 1} failed. Error: {e}")

            if attempt < ntries - 1:  # Check if we have retries left
                logging.info("Second re-try block. Sleeping before retry.")
                sleep(retry_delay)
            else:
                logging.info("Second re-try block. All retries exhausted.")

                # Store 'FAILURE' for the source code for the message
                data_posting['source_code_message'] = 'FAILURE'

                # Store 'FAILURE' for the text for the message
                data_posting['text_message'] = 'FAILURE'

                logging.info("Data for the posting stored as 'FAILURE'.")

                # Append the data for the posting to the data for the compilation
                data_compilation.append(data_posting)
                logging.info("Data for the posting appended to the data for the compilation after failure.")

    # Append the data for the posting to the data for the compilation
    data_compilation.append(data_posting)
    logging.info("Data for the posting appended to the data for the compilation after success.")

# Quit the driver
driver.quit()
logging.info("Web driver quit.")

# Create a DataFrame with the data for the compilation and save it to a .csv file
# QUEST UNCOMMENT
DataFrame(data_compilation).to_csv('/projects/p32130/gideon/data/'+week.replace(",", "").replace(" ", "_").lower()+'_'+url_id+'.csv', index = False)
# LOCAL MACHINE UNCOMMENT
# DataFrame(data_compilation).to_csv(week.replace(",", "").replace(" ", "_").lower() + '.csv', index = False)
logging.info("Data for the compilation stored in a .csv file.")

logging.info(f"Compilation URL: {compilation_url}")
logging.info(f"Compilation URL ID: {url_id}")