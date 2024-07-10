import postgres from 'postgres';
export const db = postgres({
    host: process.env.MUSICASSESSR_DB_HOST,
    port: 5432,
    database: process.env.MUSICASSESSR_DB_NAME,
    username: process.env.MUSICASSESSR_DB_USER,
    password: process.env.MUSICASSESSR_DB_PASSWORD,
    ssl: { rejectUnauthorized: false }
});
