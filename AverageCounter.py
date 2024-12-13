import pandas as pd

def process_csv(file_path):
    # Read the CSV file
    df = pd.read_csv(file_path)

    # Drop empty rows and columns
    df = df.dropna(how='all').dropna(axis=1, how='all')

    # Calculate the average of values in each row
    df['Average'] = df.mean(axis=1)

    # Save the processed DataFrame to a new CSV file (optional)
    df.to_csv('processed_' + file_path, index=False)

    return df

if __name__ == "__main__":
    # Replace 'your_file.csv' with the path to your CSV file
    file_path = 'ParallelData.csv'
    processed_df = process_csv(file_path)
    print(processed_df)
