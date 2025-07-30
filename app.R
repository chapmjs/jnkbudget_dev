from shiny.express import input, ui, render
from shiny import reactive, req
import pandas as pd
import numpy as np
from datetime import datetime, date
import calendar
import sqlite3
import os
import mysql.connector
from mysql.connector import Error

# App Configuration
APP_PASSWORD = "budget2025"  # Change this to your desired password
DATABASE_FILE = "demo_budget_data.db"  # Local SQLite for demo data
MYSQL_CONFIG = {
    'host': 'mysql.bbfarm.org',
    'database': 'budget_db',  # Change to your database name
    'user': 'budget_user',    # Change to your MySQL username
    'password': 'your_mysql_password'  # Change to your MySQL password
}

# Budget categories
BUDGET_CATEGORIES = [
    "Housing", "Food", "Clothing", "Education", "Transportation", 
    "Communications", "Health", "Recreation", "Other", "Debt", 
    "Fast Offering", "Tithing", "Income"
]

class DatabaseManager:
    def __init__(self, use_mysql=False):
        self.use_mysql = use_mysql
        self.mysql_connection = None
        
        if use_mysql:
            self.init_mysql_database()
        else:
            self.init_sqlite_database()
    
    def init_mysql_database(self):
        """Initialize MySQL database connection and create table if needed"""
        try:
            self.mysql_connection = mysql.connector.connect(**MYSQL_CONFIG)
            
            if self.mysql_connection.is_connected():
                cursor = self.mysql_connection.cursor()
                
                # Create table if it doesn't exist
                cursor.execute('''
                    CREATE TABLE IF NOT EXISTS transactions (
                        id INT AUTO_INCREMENT PRIMARY KEY,
                        date DATE NOT NULL,
                        description VARCHAR(255) NOT NULL,
                        amount DECIMAL(10, 2) NOT NULL,
                        vendor VARCHAR(255) NOT NULL,
                        budget_category VARCHAR(100) NOT NULL,
                        buyer VARCHAR(100) NOT NULL,
                        notes TEXT,
                        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                    )
                ''')
                
                self.mysql_connection.commit()
                print("✅ Connected to MySQL database successfully")
                
        except Error as e:
            print(f"❌ Error connecting to MySQL: {e}")
            # Fall back to SQLite
            self.use_mysql = False
            self.init_sqlite_database()
    
    def init_sqlite_database(self):
        """Initialize SQLite database for demo data"""
        conn = sqlite3.connect(DATABASE_FILE)
        cursor = conn.cursor()
        
        cursor.execute('''
            CREATE TABLE IF NOT EXISTS transactions (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                date TEXT NOT NULL,
                description TEXT NOT NULL,
                amount REAL NOT NULL,
                vendor TEXT NOT NULL,
                budget_category TEXT NOT NULL,
                buyer TEXT NOT NULL,
                notes TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        ''')
        
        # Check if table is empty and add sample data
        cursor.execute("SELECT COUNT(*) FROM transactions")
        count = cursor.fetchone()[0]
        
        if count == 0:
            sample_data = [
                ('2024-01-15', 'Rent Payment', -1200.00, 'Property Management', 'Housing', 'John', 'Monthly rent for apartment'),
                ('2024-01-20', 'Grocery Store', -85.50, 'Safeway', 'Food', 'Jane', 'Weekly grocery shopping'),
                ('2024-01-25', 'Gas Station', -45.00, 'Shell', 'Transportation', 'John', 'Fill up tank'),
                ('2024-02-05', 'Internet Bill', -75.00, 'Comcast', 'Communications', 'John', 'Monthly internet service'),
                ('2024-02-10', 'Restaurant', -32.00, 'Local Diner', 'Food', 'Jane', 'Dinner with friends'),
                ('2024-02-15', 'Salary', 3500.00, 'Company', 'Income', 'John', 'Bi-weekly paycheck'),
                ('2024-03-01', 'Grocery Store', -92.75, 'Walmart', 'Food', 'Jane', 'Monthly grocery shopping'),
                ('2024-03-05', 'Movie Theater', -25.00, 'AMC', 'Recreation', 'John', 'Date night'),
                ('2024-03-10', 'Doctor Visit', -150.00, 'Medical Center', 'Health', 'Jane', 'Annual checkup'),
                ('2024-03-15', 'Salary', 3500.00, 'Company', 'Income', 'John', 'Bi-weekly paycheck')
            ]
            
            cursor.executemany('''
                INSERT INTO transactions (date, description, amount, vendor, budget_category, buyer, notes)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            ''', sample_data)
        
        conn.commit()
        conn.close()
        print("📊 Using demo SQLite database")
    
    def add_transaction(self, date_val, description, amount, vendor, category, buyer, notes):
        """Add a new transaction to the database"""
        if self.use_mysql and self.mysql_connection and self.mysql_connection.is_connected():
            try:
                cursor = self.mysql_connection.cursor()
                cursor.execute('''
                    INSERT INTO transactions (date, description, amount, vendor, budget_category, buyer, notes)
                    VALUES (%s, %s, %s, %s, %s, %s, %s)
                ''', (date_val.strftime('%Y-%m-%d'), description, amount, vendor, category, buyer, notes))
                
                self.mysql_connection.commit()
                return True
            except Error as e:
                print(f"Error adding transaction to MySQL: {e}")
                return False
        else:
            # Use SQLite
            conn = sqlite3.connect(DATABASE_FILE)
            cursor = conn.cursor()
            
            cursor.execute('''
                INSERT INTO transactions (date, description, amount, vendor, budget_category, buyer, notes)
                VALUES (?, ?, ?, ?, ?, ?, ?)
            ''', (date_val.strftime('%Y-%m-%d'), description, amount, vendor, category, buyer, notes))
            
            conn.commit()
            conn.close()
            return True
    
    def get_all_transactions(self):
        """Get all transactions as a pandas DataFrame"""
        if self.use_mysql and self.mysql_connection and self.mysql_connection.is_connected():
            try:
                df = pd.read_sql_query('''
                    SELECT date, description, amount, vendor, budget_category, buyer, notes
                    FROM transactions
                    ORDER BY date DESC
                ''', self.mysql_connection)
                
                if not df.empty:
                    df['date'] = pd.to_datetime(df['date'])
                
                return df
            except Error as e:
                print(f"Error reading from MySQL: {e}")
                return pd.DataFrame()
        else:
            # Use SQLite
            conn = sqlite3.connect(DATABASE_FILE)
            
            df = pd.read_sql_query('''
                SELECT date, description, amount, vendor, budget_category, buyer, notes
                FROM transactions
                ORDER BY date DESC
            ''', conn)
            
            conn.close()
            
            if not df.empty:
                df['date'] = pd.to_datetime(df['date'])
            
            return df
    
    def clear_all_transactions(self):
        """Clear all transactions from the database"""
        if self.use_mysql and self.mysql_connection and self.mysql_connection.is_connected():
            try:
                cursor = self.mysql_connection.cursor()
                cursor.execute("DELETE FROM transactions")
                self.mysql_connection.commit()
                return True
            except Error as e:
                print(f"Error clearing MySQL transactions: {e}")
                return False
        else:
            # Use SQLite
            conn = sqlite3.connect(DATABASE_FILE)
            cursor = conn.cursor()
            cursor.execute("DELETE FROM transactions")
            conn.commit()
            conn.close()
            return True
    
    def close_connection(self):
        """Close database connections"""
        if self.mysql_connection and self.mysql_connection.is_connected():
            self.mysql_connection.close()

# Global database manager
db_manager = None

# Authentication state
authenticated = reactive.value(False)
using_mysql = reactive.value(False)

# Reactive data
budget_data = reactive.value(pd.DataFrame())

def refresh_data():
    """Refresh data from database"""
    if db_manager:
        budget_data.set(db_manager.get_all_transactions())

# Password authentication UI
def auth_ui():
    return ui.div(
        ui.div(
            ui.h2("🔒 JnK Budget Tracking Tool", style="text-align: center; margin-bottom: 30px;"),
            ui.div(
                ui.input_password("password", "Enter Password:", placeholder="Enter your password"),
                ui.input_action_button("login", "Login", class_="btn-primary", style="width: 100%; margin-top: 10px;"),
                ui.div(
                    ui.p("Demo Mode: Any incorrect password will show demo data", 
                         style="text-align: center; margin-top: 15px; color: #666; font-size: 0.9em;"),
                    ui.p("Admin Mode: Correct password connects to MySQL database", 
                         style="text-align: center; color: #666; font-size: 0.9em;")
                ),
                style="max-width: 400px; margin: 0 auto; padding: 30px; border: 1px solid #ddd; border-radius: 8px; background-color: #f9f9f9;"
            ),
            style="display: flex; flex-direction: column; justify-content: center; align-items: center; min-height: 80vh;"
        )
    )

# Main application UI
def main_ui():
    return ui.div(
        ui.div(
            ui.h1("📊 JnK Budget Tracking Tool"),
            ui.div(
                ui.output_ui("database_status"),
                ui.input_action_button("logout", "Logout", class_="btn-secondary"),
                style="float: right; margin-top: -10px;"
            ),
            style="clearfix; margin-bottom: 20px;"
        ),
        
        ui.layout_sidebar(
            ui.sidebar(
                ui.h3("Add New Transaction"),
                ui.input_date("date", "Date", value=date.today()),
                ui.input_text("description", "Description", placeholder="Enter description"),
                ui.input_numeric("amount", "Amount", value=0, step=0.01),
                ui.input_text("vendor", "Vendor", placeholder="Enter vendor name"),
                ui.input_select("category", "Budget Category", choices=BUDGET_CATEGORIES),
                ui.input_text("buyer", "Buyer", placeholder="Enter buyer name"),
                ui.input_text_area("notes", "Notes", placeholder="Enter any additional notes (optional)", rows=3),
                ui.input_action_button("add_transaction", "Add Transaction", class_="btn-primary"),
                ui.hr(),
                ui.h4("Data Management"),
                ui.input_action_button("refresh_data", "Refresh Data", class_="btn-info"),
                ui.input_action_button("clear_data", "Clear All Data", class_="btn-warning")
            ),
            
            ui.layout_columns(
                col_widths=[6, 6],
                ui.card(
                    ui.card_header("Monthly Category Totals"),
                    
                    ui.output_ui("month_selector"),
                    ui.output_data_frame("monthly_totals")
                ),
                
                ui.card(
                    ui.card_header("All Transactions"),
                    ui.output_data_frame("all_transactions")
                )
            )
        )
    )

# Conditional UI based on authentication
@render.ui
def app_content():
    if authenticated():
        return main_ui()
    else:
        return auth_ui()

# Database status indicator
@render.ui
def database_status():
    if using_mysql():
        return ui.span("🔗 MySQL Database", style="color: green; margin-right: 10px;")
    else:
        return ui.span("📊 Demo Data", style="color: orange; margin-right: 10px;")

# Authentication logic
@reactive.effect
@reactive.event(input.login)
def handle_login():
    global db_manager
    
    if input.password() == APP_PASSWORD:
        # Correct password - try to connect to MySQL
        db_manager = DatabaseManager(use_mysql=True)
        using_mysql.set(db_manager.use_mysql)
        authenticated.set(True)
        refresh_data()
    else:
        # Incorrect password - use demo data
        db_manager = DatabaseManager(use_mysql=False)
        using_mysql.set(False)
        authenticated.set(True)
        refresh_data()

@reactive.effect
@reactive.event(input.logout)
def handle_logout():
    global db_manager
    
    if db_manager:
        db_manager.close_connection()
        db_manager = None
    
    authenticated.set(False)
    using_mysql.set(False)
    budget_data.set(pd.DataFrame())

# Month selector for authenticated users
@render.ui
def month_selector():
    if not authenticated():
        return ui.div()
    
    data = budget_data()
    if len(data) == 0:
        return ui.p("No data available")
    
    # Get unique year-month combinations
    data['YearMonth'] = data['date'].dt.to_period('M')
    unique_months = sorted(data['YearMonth'].unique(), reverse=True)
    
    if len(unique_months) == 0:
        return ui.p("No data available")
    
    month_choices = {str(month): f"{calendar.month_name[month.month]} {month.year}" 
                    for month in unique_months}
    
    return ui.input_select(
        "selected_month", 
        "Select Month", 
        choices=month_choices,
        selected=str(unique_months[0]) if unique_months else None
    )

@render.data_frame
def monthly_totals():
    if not authenticated():
        return pd.DataFrame()
    
    data = budget_data()
    
    if len(data) == 0 or not hasattr(input, 'selected_month') or input.selected_month() is None:
        return pd.DataFrame(columns=['Category', 'Total', 'Transaction Count'])
    
    # Filter data for selected month
    selected_period = pd.Period(input.selected_month())
    data['YearMonth'] = data['date'].dt.to_period('M')
    monthly_data = data[data['YearMonth'] == selected_period]
    
    if len(monthly_data) == 0:
        return pd.DataFrame(columns=['Category', 'Total', 'Transaction Count'])
    
    # Calculate totals by category
    category_totals = monthly_data.groupby('budget_category').agg({
        'amount': ['sum', 'count']
    }).round(2)
    
    category_totals.columns = ['Total', 'Transaction Count']
    category_totals = category_totals.reset_index()
    category_totals.columns = ['Category', 'Total', 'Transaction Count']
    
    # Format the Total column as currency
    category_totals['Total'] = category_totals['Total'].apply(lambda x: f"${x:,.2f}")
    
    return category_totals.sort_values('Category')

@render.data_frame
def all_transactions():
    if not authenticated():
        return pd.DataFrame()
    
    data = budget_data()
    if len(data) == 0:
        return pd.DataFrame(columns=['Date', 'Description', 'Amount', 'Vendor', 'Category', 'Buyer', 'Notes'])
    
    # Format for display
    display_data = data.copy()
    display_data['amount'] = display_data['amount'].apply(lambda x: f"${x:,.2f}")
    display_data['date'] = display_data['date'].dt.strftime('%Y-%m-%d')
    display_data = display_data.rename(columns={
        'date': 'Date',
        'description': 'Description', 
        'amount': 'Amount',
        'vendor': 'Vendor',
        'budget_category': 'Category',
        'buyer': 'Buyer',
        'notes': 'Notes'
    })
    
    # Reorder columns
    column_order = ['Date', 'Description', 'Amount', 'Vendor', 'Category', 'Buyer', 'Notes']
    display_data = display_data[column_order]
    
    return display_data

@reactive.effect
@reactive.event(input.add_transaction)
def add_new_transaction():
    if not authenticated() or not db_manager:
        return
    
    req(input.description(), input.vendor(), input.buyer())
    
    # Add to database
    success = db_manager.add_transaction(
        input.date(),
        input.description(),
        input.amount(),
        input.vendor(),
        input.category(),
        input.buyer(),
        input.notes() if input.notes() else ""
    )
    
    if success:
        # Refresh data
        refresh_data()

@reactive.effect
@reactive.event(input.refresh_data)
def refresh_data_handler():
    if not authenticated():
        return
    refresh_data()

@reactive.effect
@reactive.event(input.clear_data)
def clear_all_data():
    if not authenticated() or not db_manager:
        return
    
    # Clear database
    success = db_manager.clear_all_transactions()
    
    if success:
        # Refresh data
        refresh_data()
